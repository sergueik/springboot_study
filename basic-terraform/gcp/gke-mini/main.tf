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

resource "google_container_cluster" "minimal_gke" {
  name     = "minimal-gke"
  location = var.region

  # Disable default node pool to avoid deletion issues
  remove_default_node_pool = true
  initial_node_count       = 1

  # Optional: Specify the GKE cluster master configuration
  #  initial_cluster_version = "latest"

  # Networking configurations
  network    = google_compute_network.gke_network.name
  subnetwork = google_compute_subnetwork.gke_subnet.name
}

resource "google_container_node_pool" "minimal_pool" {
  name       = "minimal-node-pool"
  location   = var.region
  cluster    = google_container_cluster.minimal_gke.name
  node_count = 1

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
}

resource "kubernetes_deployment" "nginx" {
  count = var.deploy_workload ? 1 : 0 # Deploy only if deploy_workload is true

  metadata {
    name = "nginx-deployment"
    labels = {
      app = "nginx"
    }
  }

  spec {
    replicas = 1

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
          image = "nginx:latest"
          name  = "nginx"
          port {
            container_port = 80
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "nginx" {
  count = var.deploy_workload ? 1 : 0 # Deploy only if deploy_workload is true

  metadata {
    name = "nginx-service"
  }

  spec {
    selector = {
      app = "nginx"
    }

    port {
      port        = 80
      target_port = 80
    }

    type = "LoadBalancer"
  }
}
