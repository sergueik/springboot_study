terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.5"

    }
    google = {
      source = "hashicorp/google"
      // version = "~>6.1.0"

    }

    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
/*
  cloud {
    organization = "skouzmine_testing"

    workspaces {
      name = "learn-terraform"
    }
  }
*/
}

provider "google" {
  project     = var.project
  region      = "us-central1"
  zone        = "us-central1-c"
  credentials = file("../keys.json")
}

locals {
  // prefix                = var.prefix != "" ? "${var.prefix}-" : ""
  service_accounts_list = [for account in google_service_account.service_accounts : account]
  // TODO: create zipmap of name , account. NOTE   service_accounts already is keyes by name
  service_accounts_map = zipmap(var.names,local.service_accounts_list)
  emails_map           = {for name, account in local.service_accounts_map : name =>account.email} 
  emails_list           = [for account in local.service_accounts_list : account.email]
  iam_emails_list       = [for email in local.emails_list : "serviceAccount:${email}"]
  names                 = toset(var.names)
  // prefix project roles with project ( "project=>role"  ) 

  project_roles = [ for role in var.project_roles :
    "${var.project}=>${role}"
  ]
  projects  = [ var.project ]
  project_roles_pairs       = setproduct(toset(local.projects), toset(var.project_roles))
  project_roles2 =  [for pair in local.project_roles_pairs : "${pair[0]}=>${pair[1]}"]

  name_role_pairs       = setproduct(local.names, toset(local.project_roles))
  project_roles_map_data = zipmap(
    [for pair in local.name_role_pairs : "${pair[0]}-${pair[1]}"],
    [for pair in local.name_role_pairs : {
      name = pair[0]
      role = pair[1]
    }]
  )
}

// create service accounts
resource "google_service_account" "service_accounts" {
  for_each     = local.names
  account_id   = "${lower(each.value)}"
  create_ignore_already_exists = true
  display_name = each.value
  description  = index(var.names, each.value) >= length(var.descriptions) ? var.description : element(var.descriptions, index(var.names, each.value))
  project      = var.project
}

resource "google_project_iam_member" "project-roles" {
  for_each = local.project_roles_map_data

  project = element(
    split(
      "=>",
      each.value.role
    ),
    0,
  )

  role = element(
    split(
      "=>",
      each.value.role
    ),
    1,
  )

  member = "serviceAccount:${google_service_account.service_accounts[each.value.name].email}"
}
