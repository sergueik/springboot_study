terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source = "hashicorp/google"
      // version = "~>6.1.0"

    }

    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}
# Provider block to set up Google Cloud
provider "google" {
  project     = "spheric-alcove-430818-f9"
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}


# Define a variable for the instance type
variable "instance_type" {
  default = "e2-micro"
}

# Define the network for the instances
resource "google_compute_network" "example_network" {
  name = "example-network"
}

# Define a subnetwork for the instances
resource "google_compute_subnetwork" "example_subnetwork" {
  name          = "example-subnetwork"
  ip_cidr_range = "10.0.0.0/16"
  network       = google_compute_network.example_network.id
}

# Firewall rule to allow access to the proxy server from any IP address
resource "google_compute_firewall" "allow-proxy-ingress" {
  name    = "allow-proxy-ingress"
  network = google_compute_network.example_network.id

  allow {
    protocol = "tcp"
    ports    = ["8080"]  # Proxy server port
  }

  source_ranges = ["0.0.0.0/0"]  # Allow access from any IP
  target_tags   = ["proxy-server"]
}

# Firewall rule to allow access to the web server only from the proxy server
resource "google_compute_firewall" "allow-proxy-to-web" {
  name    = "allow-proxy-to-web"
  network = google_compute_network.example_network.id

  allow {
    protocol = "tcp"
    ports    = ["80"]  # Web server port (HTTP)
  }

  # Source IP range restricted to the proxy server's private IP range
  source_tags = ["proxy-server"]  # Only allow traffic from the proxy server
  target_tags = ["web-server"]    # Applies to the web server
}

# Proxy server instance
resource "google_compute_instance" "proxy_server" {
  name         = "proxy-server"
  machine_type = var.instance_type
  tags         = ["proxy-server"]  # Apply the "proxy-server" tag
# Enable preemptible instances (spot instances)
  scheduling {
    preemptible       = true
    on_host_maintenance = "TERMINATE"  # Required for preemptible instances
  }

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  network_interface {
    subnetwork = google_compute_subnetwork.example_subnetwork.id
    access_config { }  # Allow external IP for the proxy
  }

# NGINX proxy configuration via a startup script
# When writing NGINX configuration inside the script, remember to escape the $ sign used by NGINX (\$host, etc.) to avoid conflicts with Terraform's interpolation mechanism.
  metadata_startup_script = <<-EOF
    #!/bin/bash
    apt-get update
    apt-get install -y nginx

    # Configure NGINX as a reverse proxy
    cat <<'EOT' > /etc/nginx/sites-available/default
    server {
      listen 8080;

      location / {
        proxy_pass http://${google_compute_instance.web_server.network_interface[0].network_ip};
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
      }
    }
    EOT

    # Restart NGINX to apply the changes
    systemctl restart nginx
  EOF
}

# Web server instance
resource "google_compute_instance" "web_server" {
  name         = "web-server"
  machine_type = var.instance_type
  tags         = ["web-server"]  # Apply the "web-server" tag
# Enable preemptible instances (spot instances)
  scheduling {
    preemptible       = true
    on_host_maintenance = "TERMINATE"  # Required for preemptible instances
  }
  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  network_interface {
    subnetwork = google_compute_subnetwork.example_subnetwork.id
    # Web server does not need external access, so no access_config
  }

  # Simple web server installation (using NGINX)
  metadata_startup_script = <<-EOF
    #!/bin/bash
    apt-get update
    apt-get install -y nginx
    systemctl start nginx
    systemctl enable nginx
  EOF
}

# Output variable for the public IP address of the proxy server
output "proxy_public_ip" {
  description = "The public IP address of the proxy server"
  value       = google_compute_instance.proxy_server.network_interface[0].access_config[0].nat_ip
}
