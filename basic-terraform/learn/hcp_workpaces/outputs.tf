output "instance_ami" {
  value = module.server[*].instance_ami
}

output "instance_arn" {
value       = [for server in module.server: server.instance_arn]
# https://developer.hashicorp.com/terraform/tutorials/configuration-language/troubleshooting-workflow
}

