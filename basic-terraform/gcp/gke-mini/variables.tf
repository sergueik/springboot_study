variable "project_id" {
  description = "The GCP project ID where the resources will be created."
  type        = string
}

variable "region" {
  description = "The region where the resources will be created."
  type        = string
  default     = "us-central1"
}

variable "zone" {
  description = "The zone where the cluster will be created."
  type        = string
  default     = "us-central1-a"
}

