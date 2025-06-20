// origin: https://medium.com/ovni/terraform-templating-and-loops-9a88c0786c5c
variable "endpoints" {
  type = list(any)
  default = [
    { endpoint1 = "https://endpoint-1.example.com" },
    { endpoint2 = "https://endpoint-2.example.com" },
    { endpoint3 = "https://endpoint-3.example.com" },
  ]
}
variable "config" {
  type = map(any)
  default = { "endpoint1" : "https://endpoint-1.example.com",
    "endpoint2" : "https://endpoint-2.example.com",
    "endpoint3" : "https://endpoint-3.example.com",
  }
}
// https://registry.terraform.io/providers/hashicorp/template/latest/docs/data-sources/file
data "template_file" "a" {
  template = file("${path.module}/a.json")
  count    = length(var.endpoints)
  vars = {
    endpoint = element(values(var.endpoints[count.index]), 0)
    name     = element(keys(var.endpoints[count.index]), 0)
  }
}

variable "links" {
  type = list(string)
  default = [
    "link1",
    "link2",
    "link3",
  ]
}



data "template_file" "b" {
  template = file("${path.module}/b.json")
  vars = {
    value = join(",", data.template_file.a[*].rendered)
    links = jsonencode(var.links)
  }
}


locals {
  testdata         = <<EOF
%{for endpoint in var.endpoints[*]~}
${element(keys(endpoint), 0)} ${element(values(endpoint), 0)}
%{endfor~}

EOF
  configdata       = <<EOF
%{for k, v in var.config~}
${k}  ${v}
%{endfor}

EOF
  links_directives = <<EOT
%{for ip in var.links[*]~}
server ${ip}
%{endfor~}
EOT
}
