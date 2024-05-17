output "line1" {
  # NOTE: this return un-reshuffled entry, regardles of presence of "terraform.tfvars"
  #  value = upper(local.data[0])
  value = upper(var.lines[0])
}

output "line2" {
  value = { for s in var.lines : s => upper(s) }
}

output "line3" {
  value = var.apps["foo"]
}
