


resource "null_resource" "example" {
  triggers = {
    always = timestamp()
  }
  provisioner "local-exec" {
    command = "echo ${local.jsondata} > ${path.module}/a.txt"
  }
}

// shows old contents of the file

locals {
  jsondata = "{\"a\": 234 }"
  testdata = file("${path.module}/a.txt")
  input_data = jsondecode("${local.testdata}")
}

output "result" {
  value = local.testdata
}

output "result3" {
  value = local.input_data
}

data "local_file" "a" {
  filename   = "${path.module}/a.txt"
  depends_on = [null_resource.example]
}

// https://registry.terraform.io/providers/hashicorp/local/latest/docs/data-sources/file
output "result2" {
  value = data.local_file.a.content
}
