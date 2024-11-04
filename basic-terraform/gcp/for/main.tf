# origin : https://spacelift.io/blog/terraform-for-loop
# see also: https://chatgpt.com/c/6728f0ef-a958-8003-9a00-922eb104bfce
locals {
  data     = yamldecode(file("./data.yml"))
  data_map = { "instances" : { "details1" : 1, "details2" : 2 } }
  data2_map = {
    key1 = "value1"
    key2 = "value2"
    key3 = "value3"
  }
}
// when you use a for expression in a list context with a map as the data source, 
// Terraform will extract only the values from the map and return them as a list. 
// This is a common pattern to convert a map into a list of its values
locals {
  data2_values_list = [for v in local.data2_map : v]
  key0              = keys(local.data)
  // collects values
  dev01 = [for instances in local.data_map : instances]
  dev0  = [for instances in local.data : { for instance_key, instance_value in instances : instance_key => instance_value }]
  dev1 = [for instances in local.data : { for instance_key, instance_value in instances : instance_key => instance_value
  if instance_value.env == "dev" }]
  dev2 = merge([
    for data in local.data : { for instance_key, instance_value in data :
      instance_key => instance_value
    if instance_value.env == "dev" }
  ]...)
  // https://developer.hashicorp.com/terraform/language/functions/merge
  // https://developer.hashicorp.com/terraform/language/expressions/function-calls#expanding-function-arguments
  dev3 = merge(local.dev1...)
}

output "data2_values_list" {
  value = local.data2_values_list
}
output "dev1" {
  value = local.dev1
}

/*
output "type_data" {
  value = type(local.data)
}
Error: Call to unknown function
*/


output "dev2" {
  value = keys(local.dev2)
}

