


resource "null_resource" "example" {
  triggers = {
    always = timestamp()
  }
  provisioner "local-exec" {
    command = "echo 234"

  }


}
