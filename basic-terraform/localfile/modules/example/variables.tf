variable "apps"  {
  description = "some configuration map"
  type = map(any)
}

variable "lines" {
  description = "text lines"
  type = list(string)
  
  validation {
    condition = length(var.lines) >= 3
    error_message = "At least 3 lines must be supplied."
  }
}

