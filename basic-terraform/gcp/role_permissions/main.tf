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
// https://registry.terraform.io/modules/terraform-google-modules/iam/google/latest/submodules/custom_role_iam
// first found under
// https://github.com/indrajitp/terraform-gcp-custom-roles/tree/master/modules/custom_role_iam
// https://stackoverflow.com/questions/49582977/gcp-custom-iam-role-creation-with-terraform


// https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_role

data "google_iam_role" "role_permissions" {
  for_each = toset(var.base_roles)
  name     = each.value
}

locals {
  included_permissions1 = values(data.google_iam_role.role_permissions)
  included_permissions2 = flatten(values(data.google_iam_role.role_permissions)[*].included_permissions)
}  


// see also: 
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_testable_permissions
// https://cloud.google.com/apis/design/resource_names#full_resource_name

// https://serverfault.com/questions/1043115/how-do-i-list-all-the-perms-of-a-pre-defined-role
// https://github.com/hashicorp/terraform-provider-google/issues/2652
