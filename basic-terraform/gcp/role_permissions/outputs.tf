output "custom_role_id" {
  value       = data.google_iam_role.role_permissions
  description = "permissions of the role"
}


output "custom2" {
  value       = local.included_permissions1
  description = "permissions of the role"
}

output "custom3" {
  value       = local.included_permissions2
  description = "permissions of the role"
}
