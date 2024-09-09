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

data "google_compute_zones" "available" {
}

resource "google_compute_instance" "this" {
  count        = var.instance_number
  name         = "${var.server_name}-${count.index}"
  machine_type = var.machine_type
  zone         = data.google_compute_zones.available.names[count.index % length(data.google_compute_zones.available.names)]

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  network_interface {
    network = "default"
    access_config {

    }
  }
}

variable "region" {
  type        = string
  description = "Default Region"
  default     = "us-central1"
}

variable "zone" {
  type        = string
  description = "Default Zone"
  default     = "us-central1-b"
}

variable "server_name" {
  type        = string
  description = "Name of Webserver"
  default     = "webserver"
}

variable "machine_type" {
  type        = string
  description = "Machine Type"
  default     = "e2-micro"
}

variable "instance_number" {
  type    = number
  default = 2
}

variable "network_name" {
  type    = string
  default = "default"
}