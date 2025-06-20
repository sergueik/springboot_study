terraform {
  required_providers {
    http = {
      source = "hashicorp/http"
      version = "3.4.0"
    }
  }
}

variable "endpoint" {
    type = string
}

data "http" "index" {
    url = var.endpoint
    method = "GET"
    retry {
         attempts = 2
         max_delay_ms = 1000
         min_delay_ms = 500
    }
}
