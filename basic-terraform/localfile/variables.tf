variable "apps" {
  description = "some configuration map"
  type        = map(any)
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

variable "simple_map" {
  description = "simple map exercise"
  type        = map(any)
  default = {
    "1" = "foo",
    "2" = "bar"
  }
}

variable "complex_map" {
  description = "complex map exercise"
  type        = map(any)
  default = {
    "foo" = {
      inner = ["foo1", "foo2", "foo3"]
    }
    "bar" = {
      inner = ["bar2", "bar3", "bar4"]
    }
  }
}
variable "lines" {
  description = "text lines"
  default = [
    "Lorem ipsum dolor sit amet",
    "consectetur adipiscing elit",
    "sed do eiusmod tempor incididunt",
    "ut labore et dolore magna aliqua",
    "Ut enim ad minim veniam",
    "quis nostrud exercitation ullamco laboris",
    "nisi ut aliquip ex ea commodo consequat",
  ]


  type = list(string)

  validation {
    condition     = length(var.lines) >= 3
    error_message = "At least 3 lines must be supplied."
  }
}


