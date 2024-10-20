variable "region" {
  type        = string
  description = "Default Region"
  default     = "us-central1"
}

variable "zone" {
  type        = string
  description = "Default Zone"
  default     = "us-central1-c"
}

variable "server_name" {
  type        = string
  description = "Name of Webserver"
  default     = "server"
}

variable "machine_type" {
  type        = string
  description = "Machine Type"
  default     = "e2-micro"
}

variable "static_ip" {
  type    = bool
  default = true
}
