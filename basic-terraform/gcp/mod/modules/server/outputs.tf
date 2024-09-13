output "address" {
  value = var.has_static_ip ? google_compute_address.static[0].address : ""
}
