// https://developer.hashicorp.com/terraform/cli/commands/console

variable "apps" {
  type = map(any)
  default = {
    "x" = {
      "data" = "X 1",
    },
    "y" = {
      "data" = "Y 2",
    },
    "z" = {
      "data" = "Z 3",
    },
  }
}
// hashicorp/random does not support resource type "random_app".
// resource "random_app" "example" {
resource "random_pet" "example" {
  for_each = var.apps
}
// { for key, value in var.apps : key => value if value.data == "X 1" }
// {
//  "x" = {
//    "data" = "X 1"
//  }
//}
