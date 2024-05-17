resource "local_file" "dummy" {

  filename = "b.txt"
  content  = <<-EOF
    %{for s in var.tags~}
    xxx  
    ${s}
    %{endfor~}
  EOF

}
