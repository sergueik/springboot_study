terraform {
  required_providers {
    virtualbox = {
      source  = "terra-farm/virtualbox"
      version = "0.2.2-alpha.1"
    }
  }
}
resource "virtualbox_vm" "node" {
  count = 2
  name  = format("node-%02d", count.index + 1)
  # image = "file:///home/sergueik/Downloads/alpine39.box"
  # image = "file:///c:/Users/Serguei/Downloads/alpine39.box"
  # Error:  [ERROR] Unable to fetch remote image:  unsupported scheme file
  image  = "https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box"
  cpus   = 1
  memory = "64 mib"
  # user_data = file("${path.module}/user_data")

  network_adapter {
    type = "hostonly"
    # NOTE: one cannot simply skip specifying the host_iterface, which is host-specific. the error is: 'host_interface' property not set for '#0' network adapter
    # the configuration vill pass validation
    # host_interface = "vboxnet0"
    host_interface = "VirtualBox Host-Only Ethernet Adapter #3"
  }
}
