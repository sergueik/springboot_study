# based on: https://stackoverflow.com/questions/65556325/how-to-correctly-use-each-value-in-for-each-in-terraform
locals {
  #  flat_map = transpose(zipmap(keys(var.complex_map), values(var.complex_map)[*].inner))
  flat_map = zipmap(keys(var.complex_map), values(var.complex_map)[*].inner)

}

resource "local_file" "flat" {
  for_each = var.simple_map
  content  = <<-EOF
    ${each.value} 
  EOF
  filename = "b${each.key}.txt"
}


resource "local_file" "complex" {
  for_each = local.flat_map
  content  = <<-EOF
    ${jsonencode(each.value)}
  EOF
  filename = "b${each.key}.txt"
}


