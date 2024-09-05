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

// resource "google_project_service" "project" {
// project = "spheric-alcove-430818-f9"
// service =    "storage.googleapis.com"
//}
// cannot destroy
// https://stackoverflow.com/questions/56039142/enable-apis-using-serviceusage-api-with-a-service-account

resource "google_storage_bucket" "spheric-alcove-430818-f9-bucket" {
  name     = "spheric-alcove-430818-f9-bucket"
  location = "us-central1"
} 