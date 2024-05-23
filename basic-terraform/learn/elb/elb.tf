provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}

resource "aws_elb" "var" {
  name               = "foobar-terraform-elb"
  availability_zones = var.availability_zones

  access_logs {
    bucket        = "foo"
    bucket_prefix = "bar"
    interval      = 60
  }

  listener {
    instance_port     = 8000
    instance_protocol = "http"
    lb_port           = 80
    lb_protocol       = "http"
  }


  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 3
    target              = "HTTP:8000/"
    interval            = 30
  }

  tags = {
    Name = lookup(var.tags, "foo")
  }
}
