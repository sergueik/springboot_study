output "instance_ami" {
  value = module.server[*].instance_ami
}

output "instance_arn" {
  value = module.server[*].instance_arn
}

