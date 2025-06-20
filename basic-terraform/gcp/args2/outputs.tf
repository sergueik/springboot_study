output "service_accounts_list" {
  value = local.service_accounts_list
}

output "service_accounts_emails" {
  value = local.emails_map
}

output "project_roles2"{ 
  value = local.project_roles2 
}

output "iam_emails2" {
  description = "Service account emails keyed by resource.name (id)."
  value       = zipmap(local.service_accounts_list[*].name, local.iam_emails_list)
}

