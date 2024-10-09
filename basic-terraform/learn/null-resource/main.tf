


resource "null_resource" "example" {
  triggers = {
    always = timestamp()
  }
  provisioner "local-exec" {
    command = "echo 234 > ${path.module}/a.txt"

  }


}



locals {
testdata = file("${path.module}/a.txt")
}

output "result" {
value = local.testdata 
}

data "local_file" "a" {
  filename = "${path.module}/a.txt"
}

output "result2" {
value =  data.local_file.a.content
}
