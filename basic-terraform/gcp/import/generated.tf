# __generated__ by Terraform
# Please review these resources and move them into your main configuration files.

# __generated__ by Terraform from "projects/spheric-alcove-430818-f9/zones/us-central1-c/instances/test-3"
resource "google_compute_instance" "example" {
  allow_stopping_for_update = null
  can_ip_forward            = false
  deletion_protection       = false
  description               = null
  desired_status            = null
  enable_display            = false
  hostname                  = null
  labels                    = {}
  machine_type              = "e2-micro"
  metadata                  = {}
  metadata_startup_script   = null
  min_cpu_platform          = null
  name                      = "test-3"
  project                   = "spheric-alcove-430818-f9"
  resource_policies         = []
  tags                      = []
  zone                      = "us-central1-c"
  boot_disk {
    auto_delete             = true
    device_name             = "test-3"
    disk_encryption_key_raw = null # sensitive
    interface               = null
    kms_key_self_link       = null
    mode                    = "READ_WRITE"
    source                  = "https://www.googleapis.com/compute/v1/projects/spheric-alcove-430818-f9/zones/us-central1-c/disks/test-3"
    initialize_params {
      enable_confidential_compute = false
      image                       = "https://www.googleapis.com/compute/v1/projects/debian-cloud/global/images/debian-12-bookworm-v20240910"
      labels                      = {}
      provisioned_iops            = 0
      provisioned_throughput      = 0
      resource_manager_tags       = {}
      resource_policies           = []
      size                        = 10
      storage_pool                = null
      type                        = "pd-balanced"
    }
  }
  confidential_instance_config {
    confidential_instance_type  = null
    enable_confidential_compute = false
  }
  network_interface {
    internal_ipv6_prefix_length = 0
    ipv6_address                = null
    network                     = "https://www.googleapis.com/compute/v1/projects/spheric-alcove-430818-f9/global/networks/default"
    network_ip                  = "10.128.0.45"
    nic_type                    = null
    queue_count                 = 0
    stack_type                  = "IPV4_ONLY"
    subnetwork                  = "https://www.googleapis.com/compute/v1/projects/spheric-alcove-430818-f9/regions/us-central1/subnetworks/default"
    subnetwork_project          = "spheric-alcove-430818-f9"
    access_config {
      nat_ip                 = "35.223.65.246"
      network_tier           = "PREMIUM"
      public_ptr_domain_name = null
    }
  }
  reservation_affinity {
    type = "ANY_RESERVATION"
  }
  scheduling {
    automatic_restart           = false
    instance_termination_action = "STOP"
    min_node_cpus               = 0
    on_host_maintenance         = "TERMINATE"
    preemptible                 = true
    provisioning_model          = "SPOT"
  }
  service_account {
    email  = "267861691572-compute@developer.gserviceaccount.com"
    scopes = ["https://www.googleapis.com/auth/devstorage.read_only", "https://www.googleapis.com/auth/logging.write", "https://www.googleapis.com/auth/monitoring.write", "https://www.googleapis.com/auth/service.management.readonly", "https://www.googleapis.com/auth/servicecontrol", "https://www.googleapis.com/auth/trace.append"]
  }
  shielded_instance_config {
    enable_integrity_monitoring = true
    enable_secure_boot          = false
    enable_vtpm                 = true
  }
}
