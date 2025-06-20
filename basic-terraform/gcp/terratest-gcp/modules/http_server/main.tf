
locals {
  network = element(split("-", var.subnet), 0)
}


resource "google_compute_instance" "http_server" {
  project = var.project
  zone    = var.zone

  name         = "${local.network}-apache2-instance"
  machine_type = "f1-micro"

  metadata_startup_script = "sudo apt-get update && sudo apt-get install apache2 -y && echo '<html><body><h1>Environment: ${local.network}</h1></body></html>' | sudo tee /var/www/html/index.html"

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  network_interface {
    subnetwork = var.subnet
    // subnetwork = local.network
    // subnetwork = google_compute_subnetwork.default.id

    access_config {
      # Include this section to give the VM an external ip address
    }

  }
  tags = ["http-server"]
}
