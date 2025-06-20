// based on: https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide/blob/main/chap04/local-module

terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~>6.1.0"

    }
  }
}

provider "google" {
  project     = var.project
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}

module "server" {
  source        = "./modules/server"
  name          = var.name
  zone          = var.zone
  has_static_ip = var.has_static_ip
  // machine_size = "tiny"
}
