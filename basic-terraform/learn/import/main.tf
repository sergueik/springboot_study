provider "aws" {
  region     = "us-east-2"
  access_key = ""
  secret_key = ""

}
/*
resource "aws_instance" "example" {
  ami           = "ami-09da212cf18033880"
  instance_type = "t2.nano"

  tags = {
    "Name" = "test-12"
  }
  tags_all = {
    "Name" = "test-12"
  }
  user_data_replace_on_change = false

}
*/
