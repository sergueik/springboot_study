# based on: https://github.com/terraform-in-action/manning-code

terraform {
  required_providers {
    local = {
      source = "hashicorp/local"
      version = "~> 2.5"

     }
    random = {
      source = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}
resource "random_shuffle" "random_lines" {
   input = var.lines
}

locals {
  uppercase_lines = {for s in var.lines : s => upper(s)}
}
resource "local_file" "lorem" {
  filename = "a.txt"
  content = <<-EOF
    ${join("\n",  var.lines)}
  EOF
  
}
