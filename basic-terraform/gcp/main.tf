// based on: https://github.com/terraform-in-action/manning-code
terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source  = "hashicorp/google"
      version = "4.45.0"

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
  credentials = file("keys.json")
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_network#example-usage---network-basic
resource "google_compute_network" "custom_network" {
  name                    = "custom-network"
  auto_create_subnetworks = false
  //
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_network#example-usage
resource "google_compute_subnetwork" "custom_subnetwork" {
  name          = "custom-subnetwork"
  ip_cidr_range = "10.2.0.0/24"
  region        = "us-central1"
  network       = google_compute_network.custom_network.id
}

output "custom_network-id" {
  value = google_compute_network.custom_network.id
}
