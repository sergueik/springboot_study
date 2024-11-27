terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 4.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
  }

  required_version = ">=1.3.0"
}

provider "google" {
  project     = var.project_id
  region      = var.region
  credentials = file("keys.json")

}
