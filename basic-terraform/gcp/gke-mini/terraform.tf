terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 4.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
  }

  required_version = ">=1.3.0"
}

provider "google" {
  project = var.project_id
  region  = var.region
  zone        = var.zone
  credentials = file("../keys.json")

}
# 
data "google_client_config" "default" {}

provider "kubernetes" {
  host                   = google_container_cluster.minimal_gke.endpoint
  token                  = data.google_client_config.default.access_token
  cluster_ca_certificate = base64decode(google_container_cluster.minimal_gke.master_auth[0].cluster_ca_certificate)
}
/*
provider "kubernetes" {
  host                   = google_container_cluster.minimal_gke.endpoint
  token                  = google_container_cluster.minimal_gke.master_auth[0].token
  cluster_ca_certificate = base64decode(google_container_cluster.minimal_gke.master_auth[0].cluster_ca_certificate)
}

*/
