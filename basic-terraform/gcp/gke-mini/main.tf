# Create a VPC network
resource "google_compute_network" "gke_network" {
  name                    = "gke-network"
  auto_create_subnetworks = false
}

# Create a subnetwork within the VPC network
resource "google_compute_subnetwork" "gke_subnet" {
  name          = "gke-subnet"
  region        = var.region
  network       = google_compute_network.gke_network.name
  ip_cidr_range = "10.0.0.0/24"
}

# Create the GKE cluster without default node pool
resource "google_container_cluster" "minimal_gke" {
  name     = "minimal-gke"
  location = var.region
  # location = var.zone
  # Disable the default node pool creation
  remove_default_node_pool = true
  initial_node_count       = 1
  # initial_cluster_version = "latest"
  # node_locations = [var.zone]
  network    = google_compute_network.gke_network.name
  subnetwork = google_compute_subnetwork.gke_subnet.name
}

# Create a custom node pool with exactly 1 node
resource "google_container_node_pool" "minimal_pool" {
  name     = "minimal-node-pool"
  location = var.region
  # location   = var.zone
  cluster    = google_container_cluster.minimal_gke.name
  node_count = 1 # Ensure only 1 node in the pool

  node_config {
    preemptible  = true
    machine_type = "e2-micro" # Lightweight instance for cost optimization
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]
    metadata = {
      disable-legacy-endpoints = "true"
    }
  }
  # Disable auto-scaling to ensure only 1 node is used
  autoscaling {

    min_node_count = 1
    max_node_count = 1
    # enabled = false # Disable auto-scaling
  }
}

# Define Kubernetes workload (if applicable, but this should not affect node pool size)
resource "kubernetes_deployment" "nginx" {
  count = var.deploy_workload ? 1 : 0 # Deploy only if `deploy_w
  metadata {
    name = "nginx"
    labels = {
      app = "nginx"
    }
  }

  spec {
    replicas = 1 # Set to 1 replica to avoid scaling the node pool
    selector {
      match_labels = {
        app = "nginx"
      }
    }

    template {
      metadata {
        labels = {
          app = "nginx"
        }
      }

      spec {
        container {
          name  = "nginx"
          image = "nginx:latest"
          port {
            container_port = 80
          }
        }
      }
    }
  }
}
