provider "google" {
  project = var.project_id
  region  = var.region
}

# Data source to fetch IAM policies for the specified project
data "google_iam_policy" "project_policy" {
  project = var.project_id
}

# Example: Check for too privileged roles, excluding allowed users
locals {
  # List of too privileged roles
  too_privileged_roles = [
    "roles/owner",
    "roles/editor",
    "roles/iam.serviceAccountAdmin",
    "roles/iam.serviceAccountKeyAdmin"
  ]

  # List of allowed users who can have too privileged roles
  allowed_users = [
    "user:alloweduser@example.com",
    "serviceAccount:allowed-service-account@example.iam.gserviceaccount.com"
  ]

  # Collecting violations where roles are too privileged
  violations = [
    for binding in data.google_iam_policy.project_policy.bindings : [
      for member in binding.members : {
        role   = binding.role
        member = member
      }
      if contains(local.too_privileged_roles, binding.role) && !contains(local.allowed_users, member) # Check role and exclude allowed users
    ]
    if length(binding.members) > 0
  ]
}

locals {
  # Extract all bindings that have too privileged roles
  privileged_bindings = [
    for binding in data.google_iam_policy.project_policy.bindings : binding
    if contains(var.too_privileged_roles, binding.role)
  ]

  # Collect violating users who have too privileged roles
  violating_users = flatten([
    for binding in local.privileged_bindings : [
      for member in binding.members : member
      if !contains(var.allowed_users, member)
    ]
  ])
}

output "violating_users" {
  value = local.violating_users
}

# Output the violations
output "iam_violations" {
  value = flatten(local.violations) # Flatten the list to remove nested arrays
}