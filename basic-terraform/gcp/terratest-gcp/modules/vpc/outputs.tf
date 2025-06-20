

output "network" {
  value = module.vpc.network_name
}

output "subnet" {
  value = element(module.vpc.subnets_names, 0)
}
