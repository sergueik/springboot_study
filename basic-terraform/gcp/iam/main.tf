
terraform {
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
  project = "spheric-alcove-430818-f9"
  region  = "us-central1"
  zone    = "us-central1-c"
  //   credentials = file("../keys.json")
}
// TODO: cleanup
variable "instance_type" {}
variable "instance_name" {}
variable "sa_email" {
  type        = string
  description = "Email for Service Account to receive roles (Ex. default-sa@example-project-id.iam.gserviceaccount.com)"
  default     = "terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com"
}

variable "user_email" {
  type        = string
  description = "Email for group to receive roles (Ex. user@example.com)"
  default     = "kouzmine.serguei@gmail.com"

}

/******************************************
  service_account_iam_binding variables
 *****************************************/
variable "service_account_project" {
  type        = string
  description = "Project id of the service account"
  default     = "spheric-alcove-430818-f9"
}

variable "service_account_one" {
  type        = string
  description = "First service Account to add the IAM policies/bindings"
  default     = "sa-tf-test-01@spheric-alcove-430818-f9.iam.gserviceaccount.com"
}

variable "service_account_two" {
  type        = string
  description = "First service Account to add the IAM policies/bindings"
  default     = "sa-tf-test-02@spheric-alcove-430818-f9.iam.gserviceaccount.com"
}


module "service_account_iam_binding" {
  source  = "terraform-google-modules/iam/google//modules/service_accounts_iam"
  version = "~> 8.0"

  service_accounts = [var.service_account_one, var.service_account_two]
  project          = var.service_account_project
  mode             = "additive"
  bindings = {
    "roles/iam.serviceAccountKeyAdmin" = [
      "serviceaccount:${var.sa_email}",
      "user:${var.user_email}",
    ]
    "roles/iam.serviceAccountTokenCreator" = [
      "serviceaccount:${var.sa_email}",
      "user:${var.user_email}",
    ]
  }
}
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam.html#google_service_account_iam_member
resource "google_service_account_iam_member" "test" {
  member             = "serviceaccount:terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com"
  role               = "roles/iam.serviceAccountKeyAdmin"
  service_account_id = "projects/spheric-alcove-430818-f9/serviceAccounts/sa-tf-test-01@spheric-alcove-430818-f9.iam.gserviceaccount.com"

}