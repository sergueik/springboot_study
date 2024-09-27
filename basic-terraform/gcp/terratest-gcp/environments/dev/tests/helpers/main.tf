terraform {
  required_providers {
    http = {
      source = "hashicorp/http"
      version = "3.4.0"
    }
    time = {
        source = "hashicorp/time"
    }
  }
}

variable "endpoint" {
    type = string
}

resource "null_resource" "previous" {}
resource "time_sleep" "wait_30_seconds" {
  depends_on = [null_resource.previous]

  create_duration = "30s"
}

data "http" "index" {
    url = "http://${var.endpoint}"
    method = "GET"
    depends_on = [ time_sleep.wait_30_seconds ]
}