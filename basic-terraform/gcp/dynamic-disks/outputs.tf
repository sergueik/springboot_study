output "attached_disks" {
  value = values(google_compute_disk.this)[*].name
}

output "attach_disks" {

  value = var.attach_disks
}