variable "network_interfaces" {
  type = list(object({
    network = string
    access_configs = list(object({
      nat_ip       = string
      network_tier = string
    }))
  }))
}
