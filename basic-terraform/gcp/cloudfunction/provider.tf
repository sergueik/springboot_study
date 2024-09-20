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
