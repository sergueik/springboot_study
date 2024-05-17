# based on: https://stackoverflow.com/questions/65556325/how-to-correctly-use-each-value-in-for-each-in-terraform

resource "local_file" "foo" {
  for_each = var.tags
  content  = <<-EOF
  ${each.value} 
  EOF
  filename = "b${each.key}.txt"
}
