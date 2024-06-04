// see also: https://developer.hashicorp.com/terraform/tutorials/configuration-language/sensitive-variables

output "secret" {
  value = var.secret
  // NOTE: when sensitive param is not provided, plan ends with the 
  // Error: Output refers to sensitive values
  // However, terraform.tfstate will contain the un-masked value
  sensitive = true
}

output "db_connect_string" {
  description = "database connection string"
  value       = "Server=server; Database=database; Uid=user; Pwd=${var.secret}"
  sensitive   = true
}
