variable "name" {
  type        = string
  description = "server name"
}
variable "zone" {
  type = string
}
variable "has_static_ip" {
  type        = bool
  description = "switch"
}

