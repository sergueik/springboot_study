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
// https://stackoverflow.com/questions/56039142/enable-apis-using-serviceusage-api-with-a-service-account
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#example-usage---network-basic
resource "google_storage_bucket" "bucket" {
  name     = "spheric-alcove-430818-f9-bucket"
  location = "us-central1"
  force_destroy = true
  uniform_bucket_level_access = false
  public_access_prevention = "inherited"
} 

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket_object#example-usage---network-basic
resource "google_storage_bucket_object" "picture" {
  name   = "butterfly01"
  source = "a.jpg"
  bucket = google_storage_bucket.bucket.id
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_object_access_control#example-usage---network-basic


resource "google_storage_object_access_control" "public_rule" {
  object = google_storage_bucket_object.picture.name
  bucket = google_storage_bucket.bucket.name
  role   = "READER"
  entity = "allUsers"
}
//  Error:  
// Error creating ObjectAccessControl: googleapi: Error 412: The member bindings allUsers and allAuthenticatedUsers are not allowed since public access prevention is enforced., conditionNotMet 

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_network#example-usage---network-basic
// resource "google_compute_network" "vpc_network" {
//  name = "vpc-network"
//
//}