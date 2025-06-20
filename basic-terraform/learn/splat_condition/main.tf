data "template_file" "data1" {
  count    = var.enable_new_user_data ? 0 : 1
  template = file("${path.module}/data1.txt")
  vars = {
    server_port = var.server_port
  }
}
data "template_file" "data2" {
  count    = var.enable_new_user_data ? 1 : 0
  template = file("${path.module}/data2.txt")
  vars = {
    server_port = var.server_port
  }
}