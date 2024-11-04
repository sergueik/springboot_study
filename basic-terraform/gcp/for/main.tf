# origin : https://spacelift.io/blog/terraform-for-loop
# see also: https://chatgpt.com/c/6728f0ef-a958-8003-9a00-922eb104bfce
locals {
  data = yamldecode(file("./data.yml"))
}
// when you use a for expression in a list context with a map as the data source, 
// Terraform will extract only the values from the map and return them as a list. 
// This is a common pattern to convert a map into a list of its values
locals {
  key0 = keys(local.data)
  // collects values
  dev0 = [for label, instances in local.data : instances]
  // note: the syntax [FOR:IF:VALUE ] does not  work
  // with one argument, collects values
  dev1 = [for label, instances in local.data : { for instance_key, instance_value in instances : instance_key => instance_value
  if instance_value.env == "dev" } if(label == "instancesA" || label == "instancesB")]
  
  // cryptic
  dev2 = merge([
    for data in local.data : { for instance_key, instance_value in data :
      instance_key => instance_value
    if instance_value.env == "dev" }
  ]...)
  // https://developer.hashicorp.com/terraform/language/functions/merge
  // https://developer.hashicorp.com/terraform/language/expressions/function-calls#expanding-function-arguments
  dev3 = merge(local.dev1...)
  // NOTE: merge only operates on a list or tuple 
  // see also: https://spacelift.io/blog/terraform-merge-function
}

output "dev1" {
  value = local.dev1
}



output "dev2" {
  value = keys(local.dev2)
}

output "dev3" {
  value = local.dev3
}

