variable "availability_zones" {
  type = list(string)
}
variable "tags" {
  type = map(any)
  default = {
    "foo" = "foobar-terraform-elb"
  }
}
