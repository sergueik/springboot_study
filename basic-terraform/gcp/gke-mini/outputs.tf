output "cluster_name" {
  value = google_container_cluster.minimal_gke.name
}
/*
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
*/

output "kubeconfig" {
  sensitive = true
  value     = <<EOT
apiVersion: v1
clusters:
- cluster:
    certificate-authority-data: ${google_container_cluster.minimal_gke.master_auth[0].cluster_ca_certificate}
    server: ${google_container_cluster.minimal_gke.endpoint}
  name: minimal-gke
contexts:
- context:
    cluster: minimal-gke
    user: minimal-gke-user
  name: minimal-gke
current-context: minimal-gke
kind: Config
preferences: {}
users:
- name: minimal-gke-user
  user:
    token: ${data.google_client_config.default.access_token}
EOT
}
