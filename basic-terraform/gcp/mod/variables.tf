variable "name" {
  type        = string
  description = "server name"
  default     = "server"

}
variable "zone" {
  type    = string
  default = "us-central1-c"
}
variable "has_static_ip" {
  type        = bool
  description = "switch"
  default     = false
}



variable "project" {
  type        = string
  description = "project"

}
