
output "bindings_by_role" {
  value       = local.bindings_by_role
  description = "List of bindings for entities unwinded by members."
}

output "bindings_by_conditions" {
  value       = local.bindings_by_conditions
  description = "List of bindings for entities unwinded by members."
}

output  "keys_role" {
 value = local.keys_role
}

output  "bindings_member" {
   value = local.bindings_member
}