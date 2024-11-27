import "tfplan"
import "version"

# This regular expression checks whether the Terraform version used for the plan is 0.14+, 0.15+, or 1.0+

main = rule {
  version.new(tfplan.terraform_version).greater_than("1.1.0")
}
