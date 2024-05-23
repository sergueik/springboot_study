provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}

variable "ports" {
  type    = list(number)
  default = [80, 443]
}
variable "cidr" { 
  default = "0.0.0.0/0"
}
resource "aws_security_group" "demo" {
  name = "var-demo"
  dynamic "ingress" {
    for_each = var.ports
    iterator = port
    content {
      from_port   = port.value
      to_port     = port.value
      protocol    = "TCP"
      cidr_blocks = [var.cidr]
    }
  }
}
