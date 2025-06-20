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
  region      = var.region
  zone        = var.zone
  credentials = file("../keys.json")
}
