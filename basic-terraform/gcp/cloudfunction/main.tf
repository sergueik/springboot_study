
locals {

  bucketname_short = "bucket"
  bucketname       = substr(format("%s-%s", local.bucketname_short, var.project), 0, 12)
}

// https://cloud.google.com/functions/docs/create-deploy-http-nodejs
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/cloudfunctions_function
resource "google_storage_bucket" "bucket" {
  name     = local.bucketname
  location = "US"
}
// TODO: local_exec
// there must not be folder inside index.zip 
resource "google_storage_bucket_object" "archive" {
  name   = "index.zip"
  bucket = google_storage_bucket.bucket.name
  source = "./index.zip"
}

resource "google_cloudfunctions_function" "function" {
  name        = "sample-http"
  description = "My function"
  runtime     = "nodejs16"
  region      = var.region


  available_memory_mb   = 128
  source_archive_bucket = google_storage_bucket.bucket.name
  source_archive_object = google_storage_bucket_object.archive.name
  trigger_http          = true
  entry_point           = var.entrypoint
}

// IAM entry for all users to invoke the function
resource "google_cloudfunctions_function_iam_member" "invoker" {
  project        = google_cloudfunctions_function.function.project
  region         = google_cloudfunctions_function.function.region
  cloud_function = google_cloudfunctions_function.function.name

  role   = "roles/cloudfunctions.invoker"
  member = "allUsers"
}

output "url" {
  value  = google_cloudfunctions_function.function.https_trigger_url
}