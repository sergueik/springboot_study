variable "terraform_service_account" {
  type        = string
  description = "email adress of the service account used for terraform"

}
// NOTE: gcloud auth login
// will be required
variable "project_id" {
  type        = string
  description = "ID of the project in scope"
}

variable "region" {
  type        = string
  description = "default region"
}
variable "remote_state_bucket" {
  type        = string
  description = "Name of the storage bucket for remote state. Ensure that this name hase to be unique"
}
