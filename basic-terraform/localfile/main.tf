# based on: https://github.com/terraform-in-action/manning-code
// commen
terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}
module "basic" {

  source = "./modules/example"
  lines  = var.lines
  apps   = var.apps

}
module "loops" {

  source      = "./modules/dynamic"
  simple_map  = var.simple_map
  complex_map = var.complex_map

}
