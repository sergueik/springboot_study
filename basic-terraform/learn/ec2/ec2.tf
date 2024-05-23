provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}

resource "aws_instance" "ec2" {
  ami           = "ami-0a0277ba899dd9fd3"
  instance_type = "t2.micro"
}
