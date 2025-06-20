variable "region" {
  type        = string
  description = "(optional) describe your variable"
}
variable "zone" {
  type        = string
  description = "(optional) describe your variable"
}

variable "project" {}
variable "subnet" {}
// variable "auth_token" {}
variable "total_servers" {
  type = number

  validation {
    condition     = var.total_servers % 2 != 0
    error_message = "server numbers must be an odd number"
  }
}
