provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}
/* NOTE: 
Error: Incorrect attribute value type

  on ec2.tf line 13, in resource "aws_security_group" "demo":
  13:   ingress = {
  14:    from_port = "443"
  15:    to_port  = "443"
  16:    protocol = "TCP"
  17:    cidr_blocks = [ var.ip ]
  18:   }
    +----------------
    ¦ var.ip will be known only after apply

Inappropriate value for attribute "ingress": set of object required.
*/
/*
resource "aws_security_group" "demo" { 
   name = "var-demo"
  ingress = {
   from_port = "443"
   to_port  = "443"
   protocol = "TCP"
cidr_ipv4         = "0.0.0.0/0"
#   cidr_blocks      = ["0.0.0.0/0"]
#   cidr_blocks = [ var.cidr_blocks ]
  }
}
*/
output "test" {
  value = var.cidr_blocks
}
