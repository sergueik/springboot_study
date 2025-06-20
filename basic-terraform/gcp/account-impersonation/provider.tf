
provider "google" {
  alias = "impersonation"
  scopes = [
    "https://www.googleapis.com/auth/cloud-platform",
    "https://www.googleapis.com/auth/userinfo.email",
  ]

}


#receive short-lived access token
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/service_account_access_token.html
data "google_service_account_access_token" "default" {
  provider               = google.impersonation
  target_service_account = var.terraform_service_account
  scopes                 = ["cloud-platform", "userinfo-email"]
  lifetime               = "3600s"

}


# default provider to use the the token
provider "google" {
  project         = var.project_id
  access_token    = data.google_service_account_access_token.default.access_token
  request_timeout = "60s"
}
