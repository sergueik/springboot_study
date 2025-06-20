
terraform {
  backend "local" {

  }
  # backend "remote" {
  #    hostname = "app.terraform.io"
  #   organization = "kaushikgayal"
  #   workspaces {
  #     name = "terratest-gcp"
  #   }
  # }
}
