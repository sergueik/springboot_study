// origin: https://github.com/terraform-google-modules/terraform-google-iam/blob/master/examples/custom_role_project/main.tf

terraform {
// uncomment for HCP CLI

  cloud {
    organization = "skouzmine_testing"

    workspaces {
      name = "learn-terraform"
    }
  }

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
  project = var.project 
  region  = "us-central1"
  zone    = "us-central1-c"
// uncomment for local CLI 
// comment for HCP CLI
//    credentials = file("../keys.json")
}

variable "project" {
  default = "spheric-alcove-430818-f9"
}
// TODO: cleanup - variables were created in Workspace for another CLI project
variable "instance_type" {default = ""}
variable "instance_name" {default = ""}
variable "sa1_name" {
  type        = string
  description = "First service account name"
  default     = "sa-tf-test-01"
}

variable "sa2_name" {
  type        = string
  description = "Second service account name"
  default     = "sa-tf-test-02"
}

module "custom-role-project" {
  source  = "terraform-google-modules/iam/google//modules/custom_role_iam"
  version = "~> 8.0"

  target_level         = "project"
  target_id            = var.project
  role_id              = "iamDeleter"
  base_roles           = ["roles/iam.serviceAccountAdmin"]
  permissions          = ["iam.roles.list", "iam.roles.create", "iam.roles.delete"]
  excluded_permissions = ["iam.serviceAccounts.setIamPolicy", "resourcemanager.projects.get", "resourcemanager.projects.list"]
  description          = "This is a project level custom role."
  members              = ["serviceAccount:custom-role-account-01@${var.project}.iam.gserviceaccount.com", "serviceAccount:custom-role-account-02@${var.project}.iam.gserviceaccount.com"]
}

resource "google_service_account" "custom_role_account_01" {
  account_id = var.sa1_name
  project    = var.project
}

resource "google_service_account" "custom_role_account_02" {
  account_id = var.sa2_name
  project    = var.project
}
