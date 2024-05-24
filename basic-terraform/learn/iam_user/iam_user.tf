provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}

resource "aws_iam_user" "user" {
  path  = "/bin/"
  name  = "user${count.index}"
  count = 2
}

output "arns" {
  value = aws_iam_user.user[*].arn
}
