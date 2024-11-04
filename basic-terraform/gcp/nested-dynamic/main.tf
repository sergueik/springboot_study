terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source  = "hashicorp/google"
      version = "~>6.1.0"
      # NOTE: No more than 1 "condition" blocks are allowed  with 6.1.0
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



# based on: https://chatgpt.com/c/6728f0ef-a958-8003-9a00-922eb104bfce 
# see also

# https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/storage_bucket#example-usage---life-cycle-settings-for-storage-bucket-objectst
resource "google_storage_bucket" "example" {
  name     = "example-bucket"
  location = "US"

  dynamic "lifecycle_rule" {
    for_each = var.lifecycle_rules
    content {
      action {
        type = lifecycle_rule.value.action_type
      }
      //  NOTE: Each dynamic block operates independently, so you can define the inner dynamic block within the content block of the outer one.
      dynamic "condition" {
        for_each = lifecycle_rule.value.conditions
        content {
          age                   = condition.value.age
          created_before        = condition.value.created_before
          with_state            = condition.value.with_state
          matches_storage_class = condition.value.matches_storage_class
        }
      }
    }
  }
}
