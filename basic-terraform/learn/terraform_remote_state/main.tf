// NOTE: cannot use with the default state file from current directory:

// Error: Failed to read state file
// The state file could not be read: read terraform.tfstate: 
// The process cannot access the file because another process has locked a portion of the file.

data "terraform_remote_state" "local" {
  backend = "local"
  config = {
    path = "../data/terraform.tfstate"
  }
}

output "foo" {
  value = data.terraform_remote_state.local.outputs.ami
}

// Changes to Outputs:
//  - ami = "ami-02af4904e34687a9e" -> null
//  + foo = "ami-02af4904e34687a9e"

// NOTE: https://registry.terraform.io/providers/hashicorp/external/latest
// https://github.com/hashicorp/terraform-provider-external/blob/main/examples/data-sources/external.tf
