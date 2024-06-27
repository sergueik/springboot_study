# Terraform configuration

variable "ami" {
  description = "some configuration map"
  type        = map(any)
  default = {
    "dev"  = "1",
    "prod" = "2"
  }
}

