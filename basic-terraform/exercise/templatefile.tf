output "rendered" {
  value = templatefile("${path.module}/templates/greeting.tpl", { hello = "goodnight", world = "moon" })
}




