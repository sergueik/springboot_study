locals {
  network = element(split("-", var.subnet), 0)
}

resource "google_compute_firewall" "allow-http" {
  name          = "${local.network}-allow-http-https"
  network       = local.network
  source_ranges = ["0.0.0.0/0"]

  project = var.project

  allow {
    protocol = "tcp"
    ports    = [80, 443]
  }

  target_tags = ["http-server"]
  // source_ranges = ["125.253.110.51","203.47.143.194"]
}

resource "google_compute_firewall" "allow-ssh" {
  name          = "${local.network}-allow-ssh"
  network       = local.network
  project       = var.project
  source_ranges = ["0.0.0.0/0"]

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  target_tags = ["http-server"]
  // source_ranges = ["125.253.110.51"]
}
