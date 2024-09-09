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
  zone        = "us-central1-b"
  credentials = file("../keys.json")
}
// https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide/tree/main/chap03/data-source
data "google_compute_instance" "this" {
  name = "instance-1"
}
output "ip-address" {
  value = format("IP address of existing server: %s", data.google_compute_instance.this.network_interface[0].access_config[0].nat_ip)
}
