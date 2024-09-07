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

// based on:
// https://github.com/indrajitp/terraform-gcp-custom-roles/tree/master/modules/custom_role_iam
// https://stackoverflow.com/questions/49582977/gcp-custom-iam-role-creation-with-terraform
data "google_iam_role" "role_permissions" {
  for_each = toset(var.base_roles)
  name     = each.value
}

