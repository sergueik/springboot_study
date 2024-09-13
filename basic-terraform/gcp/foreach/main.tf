// based on: https://github.com/terraform-in-action/manning-code
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

provider "google" {
  project     = "spheric-alcove-430818-f9"
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}


resource "google_compute_network" "this" {
  name                    = var.network
  auto_create_subnetworks = false
}
resource "google_compute_subnetwork" "this" {
  depends_on               = [resource.google_compute_network.this]
  network = google_compute_network.this.name
  for_each = var.subnets
  name = each.key
  region =  each.value.region
  ip_cidr_range = each.value.ip_cidr_range
  private_ip_google_access = "true"
  
}

resource "google_compute_firewall" "example" {
  count = length(var.firewall)
  name = "${google_compute_network.this.name}-${var.firewall[count.index]["name"]}" 
  network = google_compute_network.this.id
  direction =  var.firewall[count.index]["direction"]
  source_ranges = var.firewall[count.index]["source_ranges"]
  target_tags = var.firewall[count.index]["target_tags"]
  allow {
    protocol = var.firewall[count.index]["allow"]["protocol"]
    ports =  var.firewall[count.index]["allow"]["ports"]
  }

  
}