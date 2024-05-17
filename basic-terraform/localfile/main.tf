# based on: https://github.com/terraform-in-action/manning-code

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
module "example" {

  source = "./modules/example"
  lines  = var.lines
  apps   = var.apps

}
