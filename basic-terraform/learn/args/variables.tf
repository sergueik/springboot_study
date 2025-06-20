// based on: https://developer.hashicorp.com/terraform/language/functions/setproduct
variable "networks" {
  type = map(object({
    base_cidr_block = string
  }))
}

variable "subnets" {
  type = map(object({
    number = number
  }))
}