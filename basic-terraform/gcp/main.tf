// based on: https://github.com/terraform-in-action/manning-code
terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source  = "hashicorp/google"
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
  credentials = file("keys.json")
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/service_account#argument-reference
data "google_service_account" "exercise" {
  account_id = "terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com"
  project    = "spheric-alcove-430818-f9"
}

data "google_service_account_iam_policy" "exercise" {
  service_account_id = data.google_service_account.exercise.name
}

output "service_account_json" {
  value = "${jsonencode(data.google_service_account.exercise)}"
}

output "service_account" {
  value = data.google_service_account.exercise
}

output "service_account_iam_policy" {
  value = data.google_service_account_iam_policy.exercise
}
// https://stackoverflow.com/questions/47006062/how-do-i-list-the-roles-associated-with-a-gcp-service-account
/*

gcloud projects get-iam-policy "spheric-alcove-430818-f9" --flatten="bindings[].members"   --format="table(bindings.role)"   --filter="bindings.members:terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com"

ROLE
roles/compute.admin
roles/iam.serviceAccountAdmin
roles/iam.serviceAccountUser
roles/servicemanagement.admin
roles/serviceusage.serviceUsageAdmin
roles/storage.admin
*/
// TODO: https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_policy
data "google_iam_policy" "report" {
  binding {
    role = "roles/compute.admin"

    members = [
      "serviceAccount:terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com",
    ]
  }
}

output "xxx" {
  value = data.google_iam_policy.report
}

 data "google_compute_default_service_account" "default" {
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam#importing-iam-policies


// terraform import google_service_account_iam_binding.default "projects/spheric-alcove-430818-f9/serviceAccounts/terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com roles/compute.admin"

/*
 import {
  id = "projects/spheric-alcove-430818-f9/serviceAccounts/terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com"
  to = google_service_account_iam_binding.default
 }

resource "google_service_account_iam_binding" "default" {
  # (resource arguments)
}

*/