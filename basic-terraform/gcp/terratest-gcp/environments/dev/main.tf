
locals {
  env = "dev"
}

module "vpc" {
  source  = "../../modules/vpc"
  project = var.project
  env     = local.env
  region  = var.region
}

module "http_server" {
  source  = "../../modules/http_server"
  project = var.project
  zone    = var.zone
  region  = var.region
  // network = module.vpc.network
  subnet = module.vpc.subnet
  // auth_token    = var.auth_token
  total_servers = var.total_servers
}

module "firewall" {
  source  = "../../modules/firewall"
  project = var.project
  subnet  = module.vpc.subnet
}
