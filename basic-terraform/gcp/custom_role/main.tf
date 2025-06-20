// based on: https://github.com/terraform-in-action/manning-code

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

// origin: https://stackoverflow.com/questions/55400579/how-to-attach-custom-gcp-role-to-a-gcp-service-account-using-terraform
/ https://stackoverflow.com/questions/61003081/how-to-properly-create-gcp-service-account-with-roles-in-terraform
// see also: https://xebia.com/blog/how-to-name-your-google-project-iam-resources-in-terraform/
// data "google_project" "project" {}
locals {
  project_id = "spheric-alcove-430818-f9"
}


resource "google_service_account" "mservice_infra_service_account" {
  account_id   = "mserviceinfra-service-account"
  display_name = "Infrastructure Service Account"
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam_custom_role
resource "google_project_iam_custom_role" "mservice_infra_admin" {
  role_id     = "mservice_infra_admin"
  title       = "mservice_infra_admin"
  description = "Infrastructure Administrator Custom Role"
  permissions = ["compute.disks.create", "compute.firewalls.create", "compute.firewalls.delete", "compute.firewalls.get", "compute.instanceGroupManagers.get", "compute.instances.create", "compute.instances.delete", "compute.instances.get", "compute.instances.setMetadata", "compute.instances.setServiceAccount", "compute.instances.setTags", "compute.machineTypes.get", "compute.networks.create", "compute.networks.delete", "compute.networks.get", "compute.networks.updatePolicy", "compute.subnetworks.create", "compute.subnetworks.delete", "compute.subnetworks.get", "compute.subnetworks.setPrivateIpGoogleAccess", "compute.subnetworks.update", "compute.subnetworks.use", "compute.subnetworks.useExternalIp", "compute.zones.get", "container.clusters.create", "container.clusters.delete", "container.clusters.get", "container.clusters.update", "container.operations.get"]
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam#google_project_iam_binding
resource "google_project_iam_binding" "mservice_infra_binding" {
  // role = "projects/${data.google_project.project.project_id}/roles/${google_project_iam_custom_role.mservice_infra_admin.role_id}"
  // no longer need a full ?
  // role    = google_project_iam_custom_role.mservice_infra_admin.role_id
   role = "projects/${local.project_id}/roles/${google_project_iam_custom_role.mservice_infra_admin.role_id}"
  project = local.project_id
  // note the prefix 
  members = [
    "serviceAccount:${google_service_account.mservice_infra_service_account.email}",
  ]
}

// https://registry.terraform.io/modules/terraform-google-modules/iam/google/latest/submodules/custom_role_iam
// https://registry.terraform.io/modules/terraform-google-modules/iam/google/latest/submodules/service_accounts_iam

// complex, sentinel does not have code to examine modules, at least not at the tfplan scope

// https://registry.terraform.io/modules/terraform-google-modules/iam/google/latest/submodules/member_iam
// simpler but
// sentinel has no find method for modules
// other than tfconfig.module_calls

// https://github.com/terraform-google-modules/terraform-google-iam/blob/v8.0.0/examples/service_account/main.tf

// the syntax of module is too complex:
// https://github.com/terraform-google-modules/terraform-google-iam/blob/v8.0.0/modules/helper/main.tf
