resource "random_id" "instance_id" {
  byte_length = 4
}

output "random_val" {
  value = random_id.instance_id.hex
}

