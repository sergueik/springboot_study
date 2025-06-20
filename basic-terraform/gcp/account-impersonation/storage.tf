resource "google_storage_bucket" "gcp_backend_bucket" {
  provider = google
  name          = var.remote_state_bucket
  
  force_destroy = true
  project       = var.project_id
  location      = var.region
  storage_class = "STANDARD"


  versioning {
    enabled = false
  }
}

data "google_client_openid_userinfo" "me" {
  provider = google
}
