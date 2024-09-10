output "region" {
  value = values(google_compute_disk.this)[*].name
}