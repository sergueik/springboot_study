provider "google" {
  project = var.project_id
  region  = var.region
}

# Data source to fetch IAM policies for the specified project
data "google_iam_policy" "project_policy" {
  project = var.project_id
}

# Example: Check if unauthorized members are assigned any roles
locals {
  # List of unauthorized members
  unauthorized_members = ["user:unauthorized@example.com"]

  # Collecting violations
  violations = [
    for binding in data.google_iam_policy.project_policy.bindings : [
      for member in binding.members : {
        role   = binding.role
        member = member
      }
      if contains(local.unauthorized_members, member) # Check each member
    ]
    if length(binding.members) > 0
  ]
}

# Output the violations
output "iam_violations" {
  value = flatten(local.violations) # Flatten the list to remove nested arrays
}
