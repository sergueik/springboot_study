
terraform {
  required_providers {
    github = {
      // NOTE: HOSTNAME is omitted, defaults to https://registry.terraform.io
      // the hostname of the public Terraform Registry.
      // https://registry.terraform.io/providers/integrations/github/latest
      source  = "integrations/github"
      version = "4.17.0"
    }
  }
}

provider "github" {

  token = "TOKEN"
}

resource "github_repository" "example" {
  name       = "example"
  visibility = "private"
}
