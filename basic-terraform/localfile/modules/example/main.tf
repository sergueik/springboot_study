resource "random_shuffle" "random_lines" {
  input = var.lines
}

locals {
  data = { for s in var.lines : s => upper(s) }
}
resource "local_file" "lorem" {
  filename = "a.txt"
  content  = <<-EOF
    ${join("\n", var.lines)}
  EOF

}
