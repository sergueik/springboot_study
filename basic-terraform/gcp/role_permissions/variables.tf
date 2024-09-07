variable "base_roles" {
  description = "base roles"
  default = ["roles/iam.serviceAccountAdmin"]

  type = list(string)
}


