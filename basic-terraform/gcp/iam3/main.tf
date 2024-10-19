provider "google" {
  project = var.project_id
  region  = var.region
}

# Data source to fetch IAM policies for the specified project
data "google_iam_policy" "project_policy" {
  project = var.project_id
}

# Example: Check for too privileged roles
locals {
  # List of too privileged roles
  too_privileged_roles = [
    "roles/owner",
    "roles/editor",
    "roles/iam.serviceAccountAdmin",
    "roles/iam.serviceAccountKeyAdmin"
  ]

  # Collecting violations where roles are too privileged
  violations = [
    for binding in data.google_iam_policy.project_policy.bindings : [
      for member in binding.members : {
        role   = binding.role
        member = member
      }
      if contains(local.too_privileged_roles, binding.role) # Check if the role is too privileged
    ]
    if length(binding.members) > 0
  ]
}

# Output the violations
output "iam_violations" {
  value = flatten(local.violations) # Flatten the list to remove nested arrays
}
