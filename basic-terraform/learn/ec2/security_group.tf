provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}
/* NOTE: 
Error: Incorrect attribute value type

  on security_group.tf line 23, in resource "aws_security_group" "demo":
  23:   ingress = {
  24:    from_port = "443"
  25:    to_port  = "443"
  26:    protocol = "TCP"
  27:    cidr_blocks = [ var.cidr_blocks ]
  28:   }
    +--------------------
    | var.cidr_blocks will be known only after apply

Inappropriate value for attribute "ingress": set of object required.
*/
resource "aws_security_group" "demo" { 
   name = "var-demo"
  ingress = {
   from_port = "443"
   to_port  = "443"
   protocol = "TCP"
   cidr_blocks = [ var.cidr_blocks ]
  }
}
output "test" {
  value = var.cidr_blocks
}
