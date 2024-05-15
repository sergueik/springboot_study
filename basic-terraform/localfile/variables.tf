variable "apps"  {
  description = "some configuration map"
  type = map(any)
  default = {
    "foo" = {
      "data" = "1",
    },
    "bar" = {
      "data" = "2",
    },
    "baz" = {
      "data" = "3",
    },
  }
}

variable "lines" {
  description = "text lines"
  default = [
  "Lorem ipsum dolor sit amet",
  "consectetur adipiscing elit",
  "sed do eiusmod tempor incididunt",
  "ut labore et dolore magna aliqua"
]


  type = list(string)
  
  validation {
    condition = length(var.lines) >= 3
    error_message = "At least 3 lines must be supplied."
  }
}

