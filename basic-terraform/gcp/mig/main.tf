resource "google_compute_instance_template" "example" {
  name         = "instance-template-name"
  machine_type = var.machine_type
  disk {
    source_image = "debian-cloud/debian-11"
    boot         = "true"
    auto_delete  = "true"
  }
  // https://cloud.google.com/compute/docs/instances/create-use-spot#terraform
  scheduling {
    preemptible                 = "true"
    provisioning_model          = "SPOT"
    automatic_restart           = "false"
    instance_termination_action = "STOP"
  }
  network_interface {
    network = "default"
    access_config {

    }
  }
  metadata_startup_script = file(join("/", [path.module, "./startup.sh"]))

  tags = ["http-server-1"]
}

resource "google_compute_firewall" "allow_http" {
  name      = "allow-http-rule"
  direction = "INGRESS"
  network   = "default"
  allow {
    ports    = ["80"]
    protocol = "tcp"
  }
  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["http-server-1"]
  priority      = 1000

}


resource "google_compute_instance_group_manager" "example" {
  name               = "instance-group-manager-example"
  base_instance_name = "example"
  zone               = var.zone
  named_port {
    name = "web"
    port = 80
  }
  version {
    name              = "instance-template"
    instance_template = google_compute_instance_template.example.id
  }
  auto_healing_policies {
    health_check      = google_compute_health_check.example.id
    initial_delay_sec = 300

  }
}


resource "google_compute_health_check" "example" {
  name                = "health-check-example"
  timeout_sec         = 5
  check_interval_sec  = 5
  healthy_threshold   = 2
  unhealthy_threshold = 10

  http_health_check {
    request_path = "/"
    port         = 80
  }
}
// simplified ?
//  see https://cloud.google.com/load-balancing/docs/https/ext-http-lb-tf-module-examples
resource "google_compute_autoscaler" "example" {
  name   = "autoscaler-example"
  zone   = var.zone
  target = google_compute_instance_group_manager.example.id
  autoscaling_policy {
    min_replicas    = 1
    max_replicas    = 2
    cooldown_period = 300
  }
}

data "google_compute_instance_group" "test" {
  name       = "instance-group-manager-example"
  depends_on = [google_compute_autoscaler.example]
}

//  shows in the second run
output "result" {
  // NOTE:  The true and false result expressions must have consistent types. 
  // The 'true' value is set of string, but the 'false' value is string.     
  value = data.google_compute_instance_group.test.instances

  // value = length(tolist(data.google_compute_instance_group.test.instances)) > 0 ? data.google_compute_instance_group.test.instances : "no instances yet"
}
