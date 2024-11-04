# origin : https://spacelift.io/blog/terraform-for-loop
# see also: https://chatgpt.com/c/6728f0ef-a958-8003-9a00-922eb104bfce
locals {
  data = yamldecode(file("./data.yml"))
}
// when you use a for expression in a list context with a map as the data source, 
// Terraform will extract only the values from the map and return them as a list. 
// This is a common pattern to convert a map into a list of its values
locals {
  // collects values
  dev10 = [for instances in local.data : instances]
  // just one cluster, get list of atttributes
  // collects values
  dev11 = [for row in local.dev10[0] : row]
  dev12 = [for row in local.dev11 : row if row.env == "dev"]
  // https://developer.hashicorp.com/terraform/language/functions/merge
  // https://developer.hashicorp.com/terraform/language/expressions/function-calls#expanding-function-arguments
  dev13 = merge(local.dev12...)
}
/*
output "data" {
  value = local.data
}

output "dev10" {
  value = local.dev10
}


output "dev11" {
  value = local.dev11
}
*/
output "dev12" {
  value = local.dev12
}

output "dev13" {
  value = local.dev13
}

/*
output "type_data" {
  value = type(local.data)
}
Error: Call to unknown function
*/


/*output "dev2" {
  value = keys(local.dev2)
}
*/

