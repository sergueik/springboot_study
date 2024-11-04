# 
variable "lifecycle_rules" {
  type = list(object({
    action_type = string
    conditions  = list(object({
      age                   = number
      created_before        = string
      with_state            = string
      matches_storage_class = list(string)
    }))
  }))
}