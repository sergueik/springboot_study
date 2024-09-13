resource "google_compute_address" "static" {
  count = var.has_static_ip ? 1 : 0
  name  = "${var.name}-static-ipv4"
}

resource "google_compute_instance" "this" {
  name         = var.name
  zone         = var.zone
  machine_type = "e2-micro"
  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
    }
  }
  network_interface {
    network = "default"
    dynamic "access_config" {

      for_each = google_compute_address.static
      content {
        // https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_address#addres
        nat_ip = access_config.value["address"]
      }
    }

  }
  metadata_startup_script =  file(join("/", [path.module , "startup.sh"] ))
  tags = ["http-server"]
}