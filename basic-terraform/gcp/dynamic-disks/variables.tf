variable "attach_disks" {
  type        = bool
  description = "atach disk"
  default     = true

}
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

variable "disks" {
  type = map(any)
}

variable "server_name" {
  type        = string
  description = "Name of Webserver"
  default     = "dummy-server"
}

variable "machine_type" {
  type        = string
  description = "Machine Type"
  default     = "e2-micro"
}
