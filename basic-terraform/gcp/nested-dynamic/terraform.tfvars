network_interfaces = [
  {
    network        = "default"
    access_configs = [
      {
        nat_ip       = "123.45.67.89"
        network_tier = "PREMIUM"
      }
    ]
  },
  {
    network        = "custom-network"
    access_configs = [
      {
        nat_ip       = "98.76.54.32"
        network_tier = "STANDARD"
      }
    ]
  }
]
