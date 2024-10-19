variable "project_id" {
  description = "The ID of the project to check"
  type        = string
}

variable "too_privileged_roles" {
  description = "List of roles considered too privileged"
  type        = list(string)
  default     = ["roles/owner", "roles/editor"]
}

variable "allowed_users" {
  description = "List of users allowed to have privileged roles"
  type        = list(string)
  default     = ["user:alloweduser@example.com"]
}
