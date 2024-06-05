provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}

resource "aws_instance" "ec2" {
  ami           = "ami-0a0277ba899dd9fd3"
  key_name    = "mykey"
  instance_type = "t2.micro"
  connection {
    type        = "ssh"
    user        = "ec2-user"
    private_key = file("./mykey.pem")
    host        = self.public_ip
  }
  /*
   timeout - last error: dial tcp 3.149.23.24:22: i/o timeout	
   */
  provisioner "remote-exec" {
    inline = [
      "sudo amazon-linux-extras install -y nginx1.12",
      "sudo systemctl start nginx"
    ]
  }


}

