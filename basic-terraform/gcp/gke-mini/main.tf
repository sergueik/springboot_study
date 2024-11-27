resource "google_container_cluster" "minimal_gke" {
  name               = "minimal-gke-cluster"
  location           = var.region
  initial_node_count = 1

  # Enable private nodes for security (optional)
  private_cluster_config {
    enable_private_nodes    = false
    enable_private_endpoint = false
  }

  # GKE default settings
  remove_default_node_pool = true
  node_locations           = [var.zone]

  # Add node pool
  node_pool {
    name       = "spot-pool"
    node_count = 1

    node_config {
      machine_type = "e2-medium"
      preemptible  = true # Spot instance
      oauth_scopes = [
        "https://www.googleapis.com/auth/cloud-platform"
      ]
    }

    management {
      auto_repair  = true
      auto_upgrade = true
    }
  }
}


resource "kubernetes_deployment" "nginx" {
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
          ports {
            container_port = 80
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "nginx" {
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
