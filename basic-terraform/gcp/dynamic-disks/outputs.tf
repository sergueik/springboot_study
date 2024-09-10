
output "attached_disks_map" {
  value = values(google_compute_disk.this_map)[*].name
}


output "attach_disks" {
  value = length(var.disks)
}
output "attach_disks_map" {
  value = local.disks_map
}

