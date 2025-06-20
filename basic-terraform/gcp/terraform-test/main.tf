terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source  = "hashicorp/google"
      version = "4.45.0"

    }

    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
}

provider "google" {
  project     = "spheric-alcove-430818-f9"
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}


// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#attributes-reference
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#example-usage---network-basic
// alternative
// https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide/blob/main/chap07/main/bucket.tf
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#nested_website
resource "google_storage_bucket" "static_website" {
  name                        = var.bucket_name
  location                    = "us-central1"
  force_destroy               = true
  uniform_bucket_level_access = true
  # NOTE:  when set, bucket cannot be immediately deleted. The policy will need to me disabled manually
  #retention_policy {
  # retention_period = 1000
  #}
  labels = {
    "key" = "value"
  }
  website {
    main_page_suffix = "index.html"
    not_found_page   = "404.html"
  }
}
// 
// 
// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket_object#example-usage---network-basic
resource "google_storage_bucket_object" "indexpage" {
  name          = "butterfly01"
  source        = "index.html"
  bucket        = google_storage_bucket.static_website.id
  cache_control = "no-store"
  content_type  = "text/html"



}
// page_hash        = "abe45d28281cfa2a4201c9b90a143095"
// curl -I  https://storage.googleapis.com/spheric-alcove-1239340/butterfly01
// etag: "abe45d28281cfa2a4201c9b90a143095"
//
// x-goog-hash: crc32c=/jfkuQ==
// x-goog-hash: md5=q+RdKCgc+ipCAcm5ChQwlQ==

resource "google_storage_bucket_iam_binding" "binding" {
  bucket  = google_storage_bucket.static_website.name
  role    = "roles/storage.objectViewer"
  members = ["allUsers", ]
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_object_access_control#example-usage---network-basic
