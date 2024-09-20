variable "project" {
  type        = string
  description = "project"

}
variable "entrypoint" {
  type        = string
  description = "function entrypoint"
  default     = "helloWorld"
}

variable "zone" {
  type    = string
  default = "us-central1-c"
}


variable "region" {
  type    = string
  default = "us-central1"
}

