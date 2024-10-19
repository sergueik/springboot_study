provider "google" {
  project = var.project_id
  region  = var.region
}

# Data source to fetch IAM policies for the specified project
data "google_iam_policy" "project_policy" {
  project = var.project_id
}

# Example: Check if a specific role is assigned to unauthorized members
locals {
  unauthorized_members = ["user:unauthorized@example.com"]
  violations          = []

  # Loop through the IAM bindings to check for violations
  bindings = [
    for binding in data.google_iam_policy.project_policy.bindings : {
      role    = binding.role
      members = binding.members
    }
  ]

  # Collect violations
  violations = [
    for binding in local.bindings :
    binding if contains(local.unauthorized_members, element(binding.members, 0)) // Check first member only
  ]
}

# Output the violations
output "iam_violations" {
  value = local.violations
}
