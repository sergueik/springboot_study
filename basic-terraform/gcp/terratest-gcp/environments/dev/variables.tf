variable "env" {
  type = string
}
variable "project" {}
variable "subnet" {}
variable "region" {
}
variable "zone" {
}
variable "total_servers" {
  type = number
  validation {
    condition     = var.total_servers % 2 != 0
    error_message = "server numbers must be an odd number"
  }
}
variable "credentials" {
}
