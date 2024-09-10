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

resource "google_compute_disk" "this" {
  for_each = var.attach_disks ? var.disks : {}
  name     = each.key
  type     = each.value["type"]
  size     = each.value["size"]
  zone     = var.zone
}

resource "google_compute_instance" "this" {
  name         = var.server_name
  machine_type = var.machine_type
  zone         = var.zone

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  dynamic "attached_disk" {
    for_each = var.attach_disks ? var.disks : {}
    content {
      source = google_compute_disk.this[attached_disk.key].name
      mode   = attached_disk.value["mode"]
    }
  }

  network_interface {
    network = "default"
  }
}