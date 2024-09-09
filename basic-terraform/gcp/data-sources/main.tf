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
  zone        = "us-central1-b"
  credentials = file("../keys.json")
}
// https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide/tree/main/chap03/data-source

locals {
  machine_type_mapping = {
    small  = "e2-micro"
    medium = "e2-medium"
    large  = "n2-standard-2"
  }
  machine_type = local.machine_type_mapping[var.machine_size]

}
variable "machine_size" {
  type    = string
  default = "small"
  validation {
    condition     = contains(keys(local.machine_type_mapping), var.machine_size)
    error_message = "The machine size must be one of small, medium, and large."
  }
}
// terraform plan -var machine_size=tiny

resource "google_compute_address" "static" {
  count = var.static_ip ? 1 : 0
  name  = "${var.name}-ipv4-address"
}

variable "static_ip" {
  type    = bool
  default = true
}
variable "name" {
  type    = string
  default = "dummy"
}


resource "google_compute_instance" "this" {
  name         = var.name
  zone         = "us-central1-b"
  machine_type = local.machine_type

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  network_interface {
    network = "default"
    dynamic "access_config" {
      for_each = google_compute_address.static
      content {
        nat_ip = access_config.value["address"]
      }
    }
  }

  metadata_startup_script = file("${path.module}/startup.sh")
  tags                    = ["http-server", ]
}