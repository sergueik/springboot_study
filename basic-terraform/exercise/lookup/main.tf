terraform {

}
//  Single quotes are not valid. Use double quotes (") to enclose strings.
// output "one_broken" {
//   value = lookup(var.ami, 'prod')
// }
output "one" {
  value = lookup(var.ami, "prod")
}
output "two" {
  value = lookup(var.ami, "stage", "ami-xyz")
}

