// based on: https://github.com/terraform-in-action/manning-code
terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source = "hashicorp/google"
      version = "~>6.1.0"

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

// https://github.com/Pruthvi360/google-cloud-services/blob/master/create%20custom%20role%20organization%20level/customrole.tf

  // org_id      = "772284654262"
// trial accounts do not have organizations
// https://cloud.google.com/resource-manager/docs/creating-managing-organization
// organization resource is available for Google Workspace and Cloud Identity customers
resource "google_project_iam_custom_role" "custom1" {
// The argument "org_id" is required, but no definition was found.
  project     = "spheric-alcove-430818-f9"
  role_id     = "0Arole1"
  title       = "Terraform1 Custom Role"
  description = "A description"
  permissions = [ "compute.addresses.deleteInternal", "compute.addresses.get", "compute.addresses.list", "compute.addresses.use"]
}

output "role_id" {
  value = google_project_iam_custom_role.custom1.name
}

// required permissions:
// Role Administrator

data "google_project_iam_policy" "policy" {
  project  = "myproject"
}