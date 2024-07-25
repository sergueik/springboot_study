locals {
  # NOTE: can use loose notation
  raw_value = file("${path.module}/example.yaml")
  data1 = yamldecode(local.raw_value)
  # can use compact notation
  data2 = yamldecode(file("${path.module}/example.yaml"))
  normalized_value = {
    name   = tostring(try(local.data1.name, null))
    groups = try(local.data1.groups, [])
    tags = try(local.data2.tags, {})
  }
}
