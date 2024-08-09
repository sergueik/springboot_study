output "name" {
  description = "Scalar Extracted from YAML"
  value       = local.normalized_value.name
}

output "groups" {
  description = "List Extracted from YAML"
  value       = "${local.normalized_value.groups}"
}


output "tags" {
  description = "Map Extracted from YAML"
  value       = "${local.normalized_value.tags}"
}
