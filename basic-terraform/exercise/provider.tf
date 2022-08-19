terraform {
  required_providers {
    # virtualbox = {
    #  source  = "terra-farm/virtualbox"
    #  version = "0.2.2-alpha.1"
    #}
    random = {
      source  = "hashicorp/random"
      version = "3.3.2"
    }
    template = {
      source  = "hashicorp/template"
      version = "2.2.0"
    }
  }
}
