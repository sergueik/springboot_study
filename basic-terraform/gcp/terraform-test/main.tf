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
      version = "~> 3.5"
    }
  }
}

provider "google" {
  project     = "spheric-alcove-430818-f9"
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}


// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#attributes-reference
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#example-usage---network-basic
resource "google_storage_bucket" "bucket" {
  name                        = var.bucket_name
  location                    = "us-central1"
  force_destroy               = true
  uniform_bucket_level_access = false
  public_access_prevention    = "inherited"
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket_object#example-usage---network-basic
resource "google_storage_bucket_object" "indexpage" {
  name   = "butterfly01"
  source = "index.html"
  bucket = google_storage_bucket.bucket.id
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_object_access_control#example-usage---network-basic


resource "google_storage_object_access_control" "public_rule" {
  object = google_storage_bucket_object.indexpage.name
  bucket = google_storage_bucket.bucket.name
  role   = "READER"
  entity = "allUsers"
}

