
output "firewall_rule" {
  value = "${google_compute_firewall.allow-http.name}"
}
