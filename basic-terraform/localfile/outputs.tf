// https://stackoverflow.com/questions/59081428/how-to-reference-a-data-source-from-a-module-to-another-module-and-pass-it-as-a
output "line4" {
  value = module.basic.line3
}
