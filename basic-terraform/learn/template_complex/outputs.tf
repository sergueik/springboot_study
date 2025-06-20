output "json" {
  value = data.template_file.b.rendered
}
output "links" {
  value = local.links_directives
}
output "endpoints" {
  value = local.testdata
}
output "configdata" {
  value = local.configdata
}
