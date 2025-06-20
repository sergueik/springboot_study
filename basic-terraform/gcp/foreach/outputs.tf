output "region" {
  value = values(google_compute_subnetwork.this)[*].region
}