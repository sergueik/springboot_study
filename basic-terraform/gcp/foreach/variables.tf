variable "network" {
  type = string
}

variable "subnets" {
  type = map(any)
}

variable "firewall" {
  type = list(any)
}
