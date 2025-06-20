// based on: https://github.com/terraform-in-action/manning-code
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
// https://registry.terraform.io/providers/hashicorp/template/latest/docs/data-sources/file.html
data "template_file" "init" {
  template = file("${path.module}/scripts/startup.sh")
  vars = {
    db_name     = "${var.db_name}"
    db_user     = "${var.db_user}"
    db_password = "${var.db_password}"
    db_ip       = "${var.db_ip}"
  }
}
output "instance_template" {
  value = data.template_file.init.rendered
}
