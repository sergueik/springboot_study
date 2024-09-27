variable "bucket_name" {
  description = "Name of the GCP bucket. Must be unique."
  default     = null
 // NOTE: null default is bad practice:
  //  The argument "name" is required, but no definition was found.  
type        = string
}
