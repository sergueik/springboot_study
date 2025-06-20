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

variable "environments" {
  description = "environments"
  type        = list(string)
  default = [
    "dev",
    "prod",
  ]

}

variable "roles" {
  description = "roles"
  type        = list(string)
  default = [
    "viewer",
    "editor",
  ]

}

locals {
  environment_roles = setproduct(toset(var.environments), toset(var.roles))
}

// variable "environment_roles" {
//  description = "environments_roles"
//  type        = map(any)
// NOTE:
// Variables may not be used here
//  default = local.environment_roles
// }


locals {
  environment_roles_zip = zipmap(
    [for pair in local.environment_roles : "${pair[0]}-${pair[1]}"],
    [for pair in local.environment_roles : {
      envieonment = pair[0]
      role        = pair[1]
    }]
  )
}
