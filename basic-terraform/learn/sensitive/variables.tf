variable "secret" {
  description = "sensitieve data"
  type        = string
  sensitive   = true
  default     = "dummy"
}

