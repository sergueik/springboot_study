output "custom_role_id" {
  value       = data.google_iam_role.role_permissions
  description = "permissions of the role"
}
