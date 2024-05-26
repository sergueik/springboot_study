
output "secret" {
  value = var.secret
  // NOTE: when sensitive param is not provided, plan ends with the 
  // Error: Output refers to sensitive values
  // However, terraform.tfstate will contain the inmasked value
  sensitive = true
}
