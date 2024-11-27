output "cluster_name" {
  value = google_container_cluster.minimal_gke.name
}

output "kubeconfig" {
  value = <<EOT
  kubectl config set-cluster minimal-gke-cluster \
    --server=https://{google_container_cluster.minimal_gke.endpoint} \
    --certificate-authority={google_container_cluster.minimal_gke.master_auth.0.cluster_ca_certificate}

  kubectl config set-credentials minimal-gke-user \
    --token={google_container_cluster.minimal_gke.master_auth.0.token}

  kubectl config set-context minimal-gke \
    --cluster=minimal-gke-cluster \
    --user=minimal-gke-user

  kubectl config use-context minimal-gke
  EOT
}

output "auth" {
  value = google_container_cluster.minimal_gke.master_auth
}
