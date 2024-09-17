// https://stackoverflow.com/questions/72423719/terraform-count-method-with-modules
variable "create_module" {
  description = "enable module"
  default     = "true"
}

variable "region" {
  description = "AWS region"
  default     = "us-east-2"
}

variable "instance_type" {
  description = "Type of EC2 instance to provision"
  default     = "t2.micro"
}

variable "instance_name" {
  description = "EC2 instance name"
  default     = "Provisioned by CLI"
}

