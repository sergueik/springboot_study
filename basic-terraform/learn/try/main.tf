variable "filename" { 
  type = string
  // NOTE: Variables may not be used here.
  // default = "${path.module}/example.yaml"
  default = "example.yaml"
  validation {
    condition     = can(file(var.filename))
    error_message = "The filename  argument requires a valid file."
  }
}

locals {
  # NOTE: can use loose notation
  raw_value = file(var.filename)
  data1 = yamldecode(local.raw_value)
  # can use compact notation
  data2 = yamldecode(file("${path.module}/example.yaml"))
  normalized_value = {
    name   = tostring(try(local.data1.name, null))
    groups = try(local.data1.groups, [])
    tags = try(local.data2.tags, {})
  }
}
// NOTE: "var"  is reserved  name in terraform
// variable "data3" {
 /// type        = string
 // default = yamldecode(file(var.filename))
//}
