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
resource "google_compute_instance" "example" {
  name         = "example-instance"
  machine_type = "e2-medium"
  zone         = "us-central1-a"
  tags         = ["web", "production"]

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }

  dynamic "network_interface" {
    for_each = var.network_interfaces
    content {
      // https://registry.terraform.io/providers/hashicorp/google/4.22.0/docs/resources/compute_instance#network_interface
      network = network_interface.value.network
      // https://registry.terraform.io/providers/hashicorp/google/4.22.0/docs/resources/compute_instance#access_config
      dynamic "access_config" {
        for_each = network_interface.value.access_configs
        content {
          nat_ip       = access_config.value.nat_ip
          network_tier = access_config.value.network_tier
        }
      }
    }
  }
}
