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

// origin: https://github.com/terraform-google-modules/terraform-google-iam/blob/master/modules/helper/main.tf
locals {
  bindings_by_role = distinct(flatten([
    for name in var.entities
    : [
// iterate over map
      for role, members in var.bindings
      : { name = name, role = role, members = members, 
          condition = { expression = "" } }
    ]
  ]))

bindings_by_conditions = distinct(flatten([
    for name in var.entities
    : [
// iterate over list of maps
      for binding in var.conditional_bindings
      : { name = name, role = binding.role, members = binding.members, 
condition = { title = binding.title, description = binding.description, expression = binding.expression } }
    ]
  ]))

keys_role = distinct(flatten([
    for alias in var.entities
    : [
      for role, members in var.bindings
      : [
        for member in members
        : "${alias}--${role}--${member}"
      ]
    ]
  ]))

 bindings_by_member = distinct(flatten([
    for binding in local.bindings_by_role
    : [
      for member in binding["members"]
      : { name = binding["name"], role = binding["role"], member = member, condition = binding["condition"] }
    ]
  ]))


 bindings_member =  zipmap(local.keys_role, local.bindings_by_member)
// when  local.bindings_by_role is used:
//  Call to function "zipmap" failed: number of keys (12) does not match number of values (4).
}
