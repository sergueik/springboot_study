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
