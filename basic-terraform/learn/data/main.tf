provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}
// https://registry.terraform.io/providers/hashicorp/aws/latest/docs/data-sources/ami
data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["543428934710"]
  // additional
  filter {
    name   = "name"
    values = ["*Ubuntu 22.04*"]
  }

}

output "ami" {
  value = data.aws_ami.ubuntu.id

}


// NOTE: https://registry.terraform.io/providers/hashicorp/external/latest
// https://github.com/hashicorp/terraform-provider-external/blob/main/examples/data-sources/external.tf
