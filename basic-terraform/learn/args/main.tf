locals {
  // convert maps(object) variables to sets for setproduct
  networks = [
    for key, network in var.networks : {
      key        = key
      cidr_block = network.base_cidr_block
    }
  ]
  subnets = [
    for key, subnet in var.subnets : {
      key    = key
      number = subnet.number
    }
  ]

  network_subnets = [
    for pair in setproduct(local.networks, local.subnets) : {
      network_key = pair[0].key
      subnet_key  = pair[1].key
      // not creating any vpc
      // network_id  = aws_vpc.example[pair[0].key].id

      // https://developer.hashicorp.com/terraform/language/functions/cidrsubnet
      cidr_block = cidrsubnet(pair[0].cidr_block, 4, pair[1].number)
    }
  ]
}