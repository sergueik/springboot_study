// based on: https://github.com/terraform-google-modules/terraform-google-service-accounts/blob/master/variables.tf
variable "names" {
  type        = list(string)
  description = "Names of the service accounts to create."
  default     = []
}

variable "project" {
  description = "Project id"
  type        = string
}

variable "project_roles" {
  type        = list(string)
  description = "roles to apply to every service acccount"
  default     = []
}

variable "description" {
  type        = string
  description = "Default description"
  default     = ""
}

variable "descriptions" {
  type        = list(string)
  description = "List of descriptions"
  default     = []
}
