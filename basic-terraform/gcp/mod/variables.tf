
// https://developer.hashicorp.com/terraform/tutorials/configuration-language/variables

variable "name" {
  type        = string
  description = "server name"
  default     = "server"

}
variable "zone" {
  type    = string
  default = "us-central1-c"
}
variable "has_static_ip" {
  type        = bool
  description = "switch"
  default     = false
}



variable "project" {
  type        = string
  description = "project"

}

variable "resource_tags" {
  type        = map(string)
  description = "tags"
  default = {
    project     = "project"
    environment = "dev"
  }
}

variable "mapobj" {
  type = map(object({
    project = string
  }))
  description = "conplex input"
  default = {
    entry1 = {
      project = "X1"
    },
    entry2 = {
      project = "X2"
    }

  }
}

// https://developer.hashicorp.com/terraform/language/functions/regexall
variable "hostname" {
  type        = string
  description = "variable with validation"
  validation {
    condition     = length(var.hostname) < 8
    error_message = "too long"
  }

  validation {
    condition     = length(regexall("[^a-z0-9]", var.hostname)) == 0
    error_message = "invalid characters"
  }
}
