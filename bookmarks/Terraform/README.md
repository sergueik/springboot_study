### Info

  * Udemy [Hashicorp Certified Terraform Associate exam questions](https://www.udemy.com/course/hashicorp-certified-terraform-associate-practice-exam-003-quiz-experts/learn/quiz/6133512#overview)
  * Hashcorp [Prepare for Terraform Certification](https://developer.hashicorp.com/terraform/tutorials/certification-003?ajs_aid=c2f83965-ee07-4ba9-9c78-6a844db25c13&product_intent=terraform)
  * [Study Guide](https://developer.hashicorp.com/terraform/tutorials/certification-003/associate-study-003)
  * [Associate Tutorial List](https://developer.hashicorp.com/terraform/tutorials/certification-associate-tutorials-003)
  * [rules and policies](https://hashicorp-certifications.zendesk.com/hc/en-us/articles/26234761626125-Exam-appointment-rules-and-requirements)
  * [Practice exam-like questions from Pearsons](https://www.oreilly.com/member/login/?next=https%3A//learning.oreilly.com/certifications/9780138190408/) (requires a login, a 10 day free trial available)
  * [Neat Reader - epub Reader for Windows 7](https://www.neat-reader.com/download/windows-epub-reader)


### Takeaways from Terraform Pracice Exam

  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/dependencies
  * https://developer.hashicorp.com/terraform/registry/private
  * https://developer.hashicorp.com/terraform/internals/debugging
  * https://developer.hashicorp.com/terraform/language/providers/requirements#best-practices-for-provider-versions
  * https://developer.hashicorp.com/terraform/registry/modules/publish#requirements
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#replace-a-resource-with-cli
  * https://developer.hashicorp.com/terraform/language/state/locking
  * https://developer.hashicorp.com/terraform/language/expressions/strings#interpolation
  * https://developer.hashicorp.com/terraform/language/expressions/strings#indented-heredocs
  * https://developer.hashicorp.com/terraform/language/expressions/strings#directives
  * https://developer.hashicorp.com/terraform/language/expressions/strings#whitespace-stripping
    + https://stackoverflow.com/questions/73169952/terraform-how-can-i-properly-do-a-string-interpolation-in-this-code
    + https://stackoverflow.com/questions/55913451/heredoc-syntax-for-variable-values
  * https://developer.hashicorp.com/terraform/language/expressions/type-constraints#collection-types
  * https://developer.hashicorp.com/terraform/language/expressions/type-constraints#primitive-types
  * https://developer.hashicorp.com/terraform/language/expressions/type-constraints#conversion-of-complex-types
  * https://developer.hashicorp.com/terraform/language/expressions/dynamic-blocks
  * https://developer.hashicorp.com/terraform/language/expressions/types#type-conversion
  * https://developer.hashicorp.com/terraform/language/expressions/for
  * https://developer.hashicorp.com/terraform/language/expressions/version-constraints
     + pessimistic = rightmost
  * https://developer.hashicorp.com/terraform/language/expressions/version-constraints#version-constraint-syntax
  * https://developer.hashicorp.com/terraform/language/expressions/custom-conditions#input-variable-validation
  * https://developer.hashicorp.com/terraform/language/expressions/dynamic-blocks#best-practices-for-dynamic-blocks

  * https://developer.hashicorp.com/terraform/language/state/remote
  * https://developer.hashicorp.com/terraform/language/state/remote#delegation-and-teamwork
  * https://developer.hashicorp.com/terraform/language/state/remote-state-data
     + https://app.terraform.io/public/signup/account?product_intent=terraform
  * https://developer.hashicorp.com/terraform/language/state/remote-state-data#the-terraform_remote_state-data-source
     + `result = data.terraform_remote_state.vpc.outputs.result`
  * https://developer.hashicorp.com/terraform/language/state/purpose
  * https://developer.hashicorp.com/terraform/language/state/workspaces
  * https://developer.hashicorp.com/terraform/cli/workspaces#when-not-to-use-multiple-workspaces
  * https://developer.hashicorp.com/terraform/enterprise/workspaces/state
     + `tfe_ouputs`
  * https://developer.hashicorp.com/terraform/cli/workspaces
    + workspace name use in a URL path segment without escaping by remote state backend
  * https://developer.hashicorp.com/terraform/cli/workspaces#workspace-internals
  * https://developer.hashicorp.com/terraform/cloud-docs/workspaces/creating#workspace-naming  = advanced
  * https://developer.hashicorp.com/terraform/cloud-docs/workspaces
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration
  * https://developer.hashicorp.com/terraform/language/values/variables
  * https://developer.hashicorp.com/terraform/language/values/variables#variable-definition-precedence
  * https://developer.hashicorp.com/terraform/cli/commands/plan#input-variables-on-the-command-line
  * https://developer.hashicorp.com/terraform/language/values/locals
  * https://medium.com/@d3vpasha/reserved-keywords-in-terraform-f37a4cbf3a81
  * https://www.hashicorp.com/products/terraform
  * https://spacelift.io/blog/what-is-terraform-cloud
  * https://developer.hashicorp.com/terraform/language/settings/backends/local
  * https://developer.hashicorp.com/terraform/language/settings/backends/local#command-line-arguments (legacy)
  * https://developer.hashicorp.com/terraform/language/files/dependency-lock
  * https://developer.hashicorp.com/terraform/tutorials/recommended-patterns/pattern-recovery
  * https://www.terraform.io/docs/configuration/providers.html#alias-multiple-provider-configurations
  * https://developer.hashicorp.com/terraform/intro/terraform-editions#hcp-terraform
  * https://developer.hashicorp.com/terraform/intro/terraform-editions#terraform-enterprise
  * https://developer.hashicorp.com/terraform/enterprise/application-administration/registry-sharing
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/sensitive-variables
  * https://www.terraform.io/docs/configuration/types.html#conversion-of-complex-types complex
  * https://developer.hashicorp.com/terraform/tutorials/aws-get-started/aws-destroy
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel#sentinel-imports
  * https://developer.hashicorp.com/sentinel/docs/language
  * https://developer.hashicorp.com/sentinel/docs/language/lists
  * https://developer.hashicorp.com/sentinel/docs/language/maps
  * https://support.hashicorp.com/hc/en-us/articles/29861232601107-How-to-Use-the-Sentinel-Playground-to-Troubleshoot-Sentinel-Policies
  * [introduction to Sentinel Playground](https://www.youtube.com/watch?v=swtpqju6bJc)
  * https://developer.hashicorp.com/sentinel/docs/language/rules
    + the sentinel playground https://play.sentinelproject.io/p/3BJV7M65e-d
  * [Testing Terraform Sentinel Policies Using Mocks](https://www.youtube.com/watch?v=y1ChJQQATTk) - multi part  
  * https://www.hashicorp.com/resources/writing-and-testing-sentinel-policies-for-terraform - older syntax, 2019
  * https://www.hashicorp.com/resources/writing-and-testing-sentinel-policies-for-terraform - tutorial -  guide !


  * https://danielrandell93.medium.com/terraform-import-blocks-dedcdf453f45
  ### Policies Library
  
  * https://registry.terraform.io/policies/hashicorp/gcp-storage-terraform/1.0.2
  * https://registry.terraform.io/policies/hashicorp/gcp-compute-terraform/1.0.2
  * https://registry.terraform.io/policies/hashicorp/gcp-networking-terraform/1.0.2
    + https://github.com/hashicorp/policy-library-gcp-networking-terraform/blob/main/policies/ssh-is-restricted-from-internet/ssh-is-restricted-from-internet.sentinel 
      - very complex logic:
      
```
deny_public_ssh_access = rule {
	all allFirewallResources as _, firewall {
		all filter firewall.change.after.allow as _, allow {
			allow.ports contains "22"
		} as _ {
			all firewall.change.after.source_ranges as _, range {
				range not in allUnsupportedSourceRanges
			}
		}
	}
}

```      
    + https://github.com/hashicorp/policy-library-gcp-networking-terraform/blob/main/policies/private-google-access-is-enabled-for-all-vpc-subnets/private-google-access-is-enabled-for-all-vpc-subnets.sentinel 
    - complex logic
    + https://developer.hashicorp.com/sentinel/docs/language/spec#quantifier-expressions-any-all-filter-map
    + https://developer.hashicorp.com/sentinel/docs/language/rules
  * https://github.com/hashicorp/policy-library-gcp-storage-terraform/
  * https://developer.hashicorp.com/sentinel/docs/writing/testing
  * https://developer.hashicorp.com/sentinel/docs/language/spec#else-operator
  * https://developer.hashicorp.com/sentinel/docs/language/spec#quantifier-expressions-any-all-filter-map
  * https://developer.hashicorp.com/sentinel/docs/language/collection-operations
    + https://discuss.hashicorp.com/t/delta-between-two-maps-in-sentinel/47873/3
  * https://developer.hashicorp.com/sentinel/docs/terraform#examples HCP Terraform evaluates Sentinel policy evaluations immediately before cost estimation
  * https://developer.hashicorp.com/sentinel/docs/language
  * https://www.hashicorp.com/blog/terraform-sentinel-v2-imports-now-in-technology-preview
  * https://giters.com/hashicorp/terraform-sentinel-policies?ysclid=m157hw807x478278902 - good intro
   + https://github.com/hashicorp/terraform-sentinel-policies/tree/main/gcp
  *   x - non woking ?
  * https://wangpp.medium.com/hashicorp-sentinel-policies-3rd-gen-part-2-of-3-12157934d437
  * https://github.com/SPHTech-Platform/policy-library-aws
  * https://github.com/hashicorp/terraform-guides/blob/master/governance/second-generation/gcp/enforce-mandatory-labels.sentinel  - non woking  (complex)
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/opa#example-policies - rego (complex)
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel/import/tfplan
  
    + `diff`
    + `applied`
      - what does it mean in the diff key
  * https://github.com/hashicorp/policy-library-gcp-compute-terraform/blob/main/policies/ensure-oslogin-is-enabled-for-a-project/ensure-oslogin-is-enabled-for-a-project.sentinel#L24 old syntax
```
  deny_undefined_compute_instance_template_metadata = rule {
	all allComputeInstanceTemplates as _, templates {
		keys(templates.change.after) contains "metadata"
	}
}

deny_undefined_compute_instance_template_block_project_ssh_keys = rule when deny_undefined_compute_instance_template_metadata is true {
	all allComputeInstanceTemplates as _, templates {
		keys(templates.change.after.metadata) contains "block-project-ssh-keys"
	}
}

```

  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel/import/tfplan  
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel/import/tfplan#namespace-resource-diff
     + `tfplan.resources.null_resource.bar[0].diff["triggers.%"].computed` ??
      - Count keys are % for maps, and # for lists and sets
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel/import/tfplan-v2#the-resource_changes-and-resource_drift-collections
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel/import/tfplan-v2#after
    
  * https://developer.hashicorp.com/terraform/cli/config/environment-variables
  * https://developer.hashicorp.com/terraform/cli/config/environment-variables#tf_log_path
  * https://developer.hashicorp.com/terraform/cli/config/environment-variables#tf_log
  * https://www.terraform.io/docs/commands/environment-variables.html#tf_logs
    + `TRACE` is the most verbose and it is the default if `TF_LOG` is set to something other than a recognized log level.
  * https://developer.hashicorp.com/terraform/language/providers/configuration
  * https://developer.hashicorp.com/terraform/language/meta-arguments/module-providers
  * https://developer.hashicorp.com/terraform/language/meta-arguments/for_each
  * https://developer.hashicorp.com/terraform/language/meta-arguments/count
  * https://developer.hashicorp.com/terraform/language/providers/requirements
  * https://developer.hashicorp.com/terraform/registry/api-docs
    + `https://modules.example.com/v1/{namespace}/{name}/{provider}/versions`
  * https://developer.hashicorp.com/terraform/registry/api-docs#list-modules
    + `https://registry.terraform.io/v1/modules?limit=2&verified=true`
  * https://developer.hashicorp.com/terraform/registry/api-docs#search-modules
    + `https://registry.terraform.io/v1/modules/search?q=network&limit=2`
    + `https://registry.terraform.io/v1/modules/hashicorp/consul/aws/versions`
    + `https://registry.terraform.io/v1/modules/hashicorp/consul/aws/0.0.1/download`
  * https://registry.terraform.io/browse/modules?provider=google  
  * https://developer.hashicorp.com/terraform/cli/init#reinitializing-only-modules
     + get
  * https://registry.terraform.io/modules/hashicorp/dir/template/latest
  * https://github.com/hashicorp/terraform-template-dir/tree/master
  * https://developer.hashicorp.com/terraform/cli/cloud/settings#arguments
  * https://developer.hashicorp.com/terraform/cli/init#working-directory-contents
  * https://developer.hashicorp.com/terraform/cloud-docs/registry/using
  * https://developer.hashicorp.com/terraform/cloud-docs/registry#private-providers-and-modules
  * https://developer.hashicorp.com/terraform/language/providers/configuration
    + `<HOSTNAME>/<ORGANIZATION>/<MODULE_NAME>/<PROVIDER_NAME>`
  * https://developer.hashicorp.com/terraform/registry/modules/use#private-registry-module-sources
  * https://developer.hashicorp.com/terraform/language/modules/sources
  * https://developer.hashicorp.com/terraform/language/modules/develop/refactoring
  * https://developer.hashicorp.com/terraform/language/modules/sources#generic-git-repository
  * https://developer.hashicorp.com/terraform/language/modules/syntax#accessing-module-output-values
  * https://www.terraform.io/docs/configuration/modules.html#module-versions
  * https://www.terraform.io/docs/configuration/modules.html#passing-providers-explicitly
  * https://spacelift.io/blog/terraform-files#project-structure-and-file-types-explained
  * https://developer.hashicorp.com/terraform/language/settings/backends/s3
  * https://developer.hashicorp.com/terraform/language/settings/backends/s3#dynamodb-table-permissions
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#examine-state-with-cli
  * https://www.hashicorp.com/blog/infrastructure-as-code-in-a-private-or-public-cloud/#IaC-Makes-Infrastructure-More-Reliable
  * https://www.udemy.com/course/hashicorp-certified-terraform-associate-practice-exam-003-quiz-experts/learn/quiz/6133512/result/1285870410#notes
  * https://www.terraform.io/docs/backends/index.html
  * https://developer.hashicorp.com/terraform/tutorials/state/refresh
  * https://developer.hashicorp.com/terraform/tutorials/modules/module-create
  * https://developer.hashicorp.com/terraform/language/values/variables
  * https://developer.hashicorp.com/terraform/intro/use-cases#multi-cloud-deployment
  * https://www.terraform.io/docs/configuration/expressions.html#best-practices-for-dynamic-blocks
  * https://www.terraform.io/docs/registry/modules/publish.html
  * https://www.terraform.io/docs/configuration/providers.html#selecting-alternate-provider-configurations
  * https://developer.hashicorp.com/terraform/language/files/dependency-lock
  * https://www.terraform.io/docs/commands/init.html#backend-initialization
  * https://www.terraform.io/docs/registry/private.html
  * https://www.terraform.io/docs/state/purpose.html#performance
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/policy-results#policy-evaluation-run-stages
  * https://developer.hashicorp.com/terraform/cloud-docs/run/states
  * https://learn.hashicorp.com/tutorials/terraform/dependencies
  * https://www.terraform.io/docs/internals/debugging.html
  * https://www.terraform.io/docs/enterprise/index.html
  * https://www.hashicorp.com/products/terraform/pricing (see Features)
     + Cross-organisation registry sharing
     + Log forwarding
  * https://www.hashicorp.com/blog/detecting-and-managing-drift-with-terraform/
  * https://developer.hashicorp.com/terraform/language/resources/syntax
    + resource configuration arguments, resource type, name, resource exported attributes
  * https://developer.hashicorp.com/terraform/language/resources/syntax#meta-arguments
  + https://developer.hashicorp.com/terraform/language/providers/requirements#source-addresses
  * https://developer.hashicorp.com/terraform/language/providers/configuration
  * https://developer.hashicorp.com/terraform/language/providers/configuration#alias-multiple-provider-configurations
  * https://developer.hashicorp.com/terraform/language/providers/configuration#selecting-alternate-provider-configurations
  * https://developer.hashicorp.com/terraform/language/resources/provisioners/syntax

    + to perform bootstrapping / destroying of a system
    + https://developer.hashicorp.com/terraform/language/resources/provisioners/local-exec
    + https://developer.hashicorp.com/terraform/language/resources/provisioners/remote-exec
    + https://developer.hashicorp.com/terraform/language/resources/provisioners/file
  * https://developer.hashicorp.com/terraform/language/resources/provisioners/null_resource
  * https://developer.hashicorp.com/terraform/language/resources/terraform-data
  * https://developer.hashicorp.com/terraform/language/meta-arguments/lifecycle#replace_triggered_by
  * https://developer.hashicorp.com/terraform/language/resources/provisioners/connection
  * https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/instance#encrypted
  * https://developer.hashicorp.com/terraform/language/v1.1.x/settings/backends
  * https://www.terraform.io/docs/commands/state/mv.html#usage
     - advanced
  * https://developer.hashicorp.com/terraform/cli/state/resource-addressing
     - advanced
  * https://www.terraform.io/intro/use-cases.html#multi-cloud-deployment
  * https://learn.hashicorp.com/tutorials/terraform/aws-build#format-and-validate-the-configuration
  * https://developer.hashicorp.com/terraform/language/meta-arguments/module-providers
  * https://registry.terraform.io/providers/hashicorp/vault/latest/docs#using-vault-credentials-in-terraform-configuration
  * https://www.terraform.io/docs/commands/state/rm.html
  * https://www.terraform.io/docs/state/remote.html
  * https://www.terraform.io/docs/state/sensitive-data.html
  * https://www.terraform.io/docs/configuration/providers.html#provider-versions
  * https://www.terraform.io/intro/index.html#infrastructure-as-code
  * https://www.terraform.io/docs/modules/sources.html
  * https://www.terraform.io/docs/commands/import.html#usage
  * https://developer.hashicorp.com/terraform/language/files/dependency-lock#new-version-of-an-existing-provider
    - need to test
  * https://www.terraform.io/docs/modules/sources.html#selecting-a-revision
    - NOTE: This is an important topic for the exam
  * https://www.terraform.io/docs/internals/debugging.html
  * https://developer.hashicorp.com/terraform/cloud-docs/cost-estimation
  * https://developer.hashicorp.com/terraform/tutorials/cloud-get-started/cost-estimation
  * https://www.terraform.io/docs/state/locking.html
  * https://www.terraform.io/docs/modules/sources.html
  * https://www.terraform.io/docs/commands/validate.html
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/variables#map-resource-tags
  * https://www.terraform.io/docs/configuration/provider-requirements.html#names-and-addresses
  * https://developer.hashicorp.com/terraform/language/values/variables#custom-validation-rules
  * https://www.terraform.io/docs/configuration/functions.html
  * https://developer.hashicorp.com/terraform/language/functions/ 
  * https://developer.hashicorp.com/terraform/language/functions/list
  * https://developer.hashicorp.com/terraform/language/functions/tostring#examples
  * https://developer.hashicorp.com/terraform/language/functions/setproduct
  * https://developer.hashicorp.com/terraform/language/functions/setintersection
  * https://developer.hashicorp.com/terraform/language/functions/setsubtract
  * https://developer.hashicorp.com/terraform/language/functions/setunion
  * https://developer.hashicorp.com/terraform/language/functions/values
  * https://developer.hashicorp.com/terraform/language/functions/transpose
  * https://developer.hashicorp.com/terraform/language/functions/zipmap
  * https://www.terraform.io/docs/configuration/functions/element.html
     - index greater than the length of the list is "wrapped around" modulo the length of the list
  * https://developer.hashicorp.com/terraform/language/functions/can
  * https://developer.hashicorp.com/terraform/language/functions/cidrhost
  * https://developer.hashicorp.com/terraform/language/functions/cidrsubnet complex
  
    + https://cidr.xyz
    + https://ntwobike.medium.com/how-cidrsubnet-works-in-terraform-f6ccd8e1838f - complex when spread among 2 octals and  have 1s to clear
    + https://truesparrow.com/blog/calculate-cidr-subnet-block-with-terraform
  * https://developer.hashicorp.com/terraform/language/functions/try
  * https://developer.hashicorp.com/terraform/language/functions/yamldecode
  * https://developer.hashicorp.com/terraform/language/functions/element
  * https://www.terraform.io/docs/commands/providers.html
  * https://www.terraform.io/docs/configuration/types.html#conversion-of-primitive-types
  * https://developer.hashicorp.com/terraform/tutorials/modules/module-use
  * https://developer.hashicorp.com/terraform/language/modules
  * https://developer.hashicorp.com/terraform/language/modules/syntax#accessing-module-output-values
  * https://developer.hashicorp.com/terraform/language/modules/syntax#version
  * https://developer.hashicorp.com/terraform/language/modules/syntax#calling-a-child-module
     - called module can not access all parent module variables - to pass variables to a child module the calling module should pass specific values in the module block
  * https://www.terraform.io/docs/commands/force-unlock.html
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#replace-a-resource-with-cli
  * https://spacelift.io/blog/terraform-architecture
  * https://www.terraform.io/docs/configuration/variables.html#assigning-values-to-root-module-variables
  * https://www.terraform.io/docs/state/remote.html#delegation-and-teamwork
  * https://www.terraform.io/docs/configuration/types.html#structural-types
  * https://www.terraform.io/docs/commands/show.html
  * https://www.terraform.io/docs/configuration/modules.html#module-versions
  * https://www.terraform.io/docs/commands/refresh.html
  * https://developer.hashicorp.com/terraform/tutorials/state/refresh
  * https://learn.hashicorp.com/tutorials/terraform/state-import#limitations-and-other-considerations
  * https://www.terraform.io/docs/registry/modules/publish.html
  * https://www.terraform.io/docs/registry/modules/use.html#private-registry-module-sources
  * https://www.terraform.io/docs/configuration/variables.html
  * https://www.terraform.io/docs/configuration/modules.html
  * https://www.terraform.io/docs/configuration/providers.html#alias-multiple-provider-configurations
  * https://www.terraform.io/docs/registry/private.html#terraform-cloud-39-s-private-registry
  * https://www.hashicorp.com/resources/how-terraform-help-multicloud-disaster-recovery/
  * https://www.terraform.io/docs/configuration/providers.html
  * https://www.terraform.io/docs/providers/index.html
  * https://www.terraform.io/docs/configuration/variables.html#assigning-values-to-root-module-variables
  * https://www.terraform.io/docs/commands/taint.html#example-tainting-a-resource-within-a-module
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#replace-a-resource-with-cli
  * https://www.terraform.io/docs/commands/validate.html
  * https://www.terraform.io/docs/state/sensitive-data.html
  * https://www.terraform.io/docs/cloud/sentinel/index.html#sentinel-in-terraform-cloud
  * https://www.terraform.io/docs/configuration/modules.html#providers-within-modules
    + although provider configurations are shared between modules, each module must declare its own provider requirement
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#replace-a-resource-with-cli
  * https://www.terraform.io/docs/configuration/types.html#conversion-of-complex-types
  * https://www.hashicorp.com/blog/infrastructure-as-code-in-a-private-or-public-cloud/
  * https://www.terraform.io/docs/commands/environment-variables.html
  * https://learn.hashicorp.com/tutorials/terraform/dependencies
  * https://www.terraform.io/docs/extend/community/contributing.html
  * https://www.terraform.io/docs/commands/plan.html
  * https://www.terraform.io/docs/commands/destroy.html
  * https://www.terraform.io/docs/commands/validate.html
  * https://www.terraform.io/docs/commands/state/list.html
  * https://www.terraform.io/docs/commands/import.html
  * https://www.terraform.io/docs/commands/apply.html
  * https://www.hashicorp.com/products/terraform/pricing

![features](https://github.com/sergueik/puppetmaster_vagrant/blob/master/Terraform/terraform_features_plans.png)

  * https://www.terraform.io/docs/commands/state/show.html
  * https://www.terraform.io/docs/state/locking.html
  * https://www.terraform.io/docs/commands/state/index.html#backups
  * https://developer.hashicorp.com/terraform/language/state#format
  * https://www.terraform.io/docs/state/remote.html
  * https://www.terraform.io/docs/configuration/expressions.html#dynamic-blocks
  * https://www.terraform.io/docs/commands/show.html
  * https://www.terraform.io/docs/state/purpose.html#mapping-to-the-real-world
  * https://www.terraform.io/docs/configuration/version-constraints.html
  * https://www.terraform.io/docs/configuration/functions/min.html
  * https://www.terraform.io/intro/use-cases.html#software-defined-networking
  * https://www.terraform.io/docs/cloud/workspaces/index.html (redirects to https://developer.hashicorp.com/terraform/cloud-docs/workspaces)
  * https://www.terraform.io/docs/import/usage.html
  * https://www.terraform.io/docs/commands/plan.html
  * https://developer.hashicorp.com/terraform/tutorials/state/resource-targeting
  * https://developer.hashicorp.com/terraform/language/import/generating-configuration +?
  * https://www.terraform.io/docs/import/index.html#currently-state-only
  * https://www.terraform.io/docs/state/workspaces.html#workspace-internals
  * https://www.hashicorp.com/blog/using-terraform-to-improve-infrastructure-security
  * https://www.terraform.io/docs/configuration/modules.html#other-meta-arguments
  * https://www.terraform.io/docs/internals/debugging.html
  * https://learn.hashicorp.com/tutorials/terraform/count
  * https://developer.hashicorp.com/terraform/tutorials/0-13/count
  * https://www.hashicorp.com/blog/using-terraform-to-improve-infrastructure-security
  * https://www.terraform.io/docs/modules/sources.html#s3-bucket
  * https://www.terraform.io/docs/configuration/modules.html#multiple-instances-of-a-module
  * https://www.terraform.io/docs/configuration/types.html#conversion-of-primitive-types
  * https://www.terraform.io/docs/commands/providers.html
  * https://www.terraform.io/docs/state/purpose.html
  * [decompose Monolithic Terraform Configurations into modules](https://youtu.be/G_vYAwc3c00)
  * https://www.terraform.io/docs/backends/index.html
  * https://www.terraform.io/docs/state/workspaces.html
  * https://www.terraform.io/docs/state/workspaces.html#workspace-internals
  * https://www.terraform.io/docs/state/purpose.html
  * https://developer.hashicorp.com/terraform/cli/auth
  * https://developer.hashicorp.com/terraform/cli/commands/login
  * https://developer.hashicorp.com/terraform/cli/commands/logout
  * https://developer.hashicorp.com/terraform/cli/commands/console
    + `.terraform.tfstate.lock.info`
  * https://developer.hashicorp.com/terraform/cli/commands/state/mv
  * https://developer.hashicorp.com/terraform/cli/commands/state/rm
  * https://developer.hashicorp.com/terraform/cli/commands/force-unlock
  * https://developer.hashicorp.com/terraform/cli/commands/show
  * https://developer.hashicorp.com/terraform/cli/import/usage
  * https://developer.hashicorp.com/terraform/cli/commands/force-unlock
  * https://developer.hashicorp.com/terraform/cli/commands/init
  * https://developer.hashicorp.com/terraform/cli/commands/fmt
  * https://developer.hashicorp.com/terraform/cli/commands/validate
  * https://developer.hashicorp.com/terraform/cli/commands/init#plugin-installation
  * https://developer.hashicorp.com/terraform/cli/commands/output
  * https://developer.hashicorp.com/terraform/cli/commands/init#child-module-installation
  * https://developer.hashicorp.com/terraform/cli/config/config-file#provider-installation
  * https://developer.hashicorp.com/terraform/cli/commands/workspace/delete
  * https://developer.hashicorp.com/terraform/language/settings/backends/oss
  * https://developer.hashicorp.com/terraform/language/settings/backends/pg
  * https://developer.hashicorp.com/terraform/language/settings/backends/local
  * https://developer.hashicorp.com/terraform/language/settings/backends/remote
  * https://developer.hashicorp.com/terraform/language/settings/backends/s3
  * https://developer.hashicorp.com/terraform/language/settings/backends/consul
    + Terraform v1.3 removed `artifactory`, `etcd`, `etcdv3`, `manta`, `swift` backends
  * https://developer.hashicorp.com/terraform/tutorials/cloud-get-started/cloud-vcs-change
  * https://developer.hashicorp.com/terraform/intro/core-workflow
  * https://developer.hashicorp.com/terraform/cloud-docs/run/cli
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams#managing-workspace-access
    + uses the highest permission level from your teams to determine what actions you can take on a particular resource
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/opa
  * https://developer.hashicorp.com/terraform/language/meta-arguments/lifecycle
  * https://developer.hashicorp.com/terraform/tutorials/state/resource-lifecycle
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration#credentials-and-sensitive-data
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration#partial-configuration
  * https://developer.hashicorp.com/terraform/cloud-docs/overview
  * https://www.hashicorp.com/products/terraform/pricing
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/api-tokens
  * https://developer.hashicorp.com/terraform/cloud-docs/api-docs/organization-tokens
  * https://developer.hashicorp.com/terraform/enterprise/api-docs/team-tokens
  * https://developer.hashicorp.com/terraform/intro/core-workflow ~
  * https://developer.hashicorp.com/terraform/cli/auth
  * https://developer.hashicorp.com/terraform/tutorials/modules/module-private-registry-share
  * https://www.terraform.io/docs/configuration/modules.html#module-versions
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/provider-versioning
  * https://developer.hashicorp.com/terraform/tutorials/0-13/cloud-login
  * https://developer.hashicorp.com/terraform/cloud-docs/run/cli
  * https://developer.hashicorp.com/terraform/cli/commands/console
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration#using-a-backend-block
  * https://developer.hashicorp.com/terraform/cloud-docs/workspaces complex
  * https://developer.hashicorp.com/terraform/cli/commands/state/rm
  * https://developer.hashicorp.com/terraform/tutorials/modules
  * https://developer.hashicorp.com/terraform/language/providers
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration
  * https://developer.hashicorp.com/terraform/cloud-docs/run/cli
  * https://developer.hashicorp.com/terraform/cli/cloud/settings#the-cloud-block
  * https://developer.hashicorp.com/terraform/language/values/locals
  * https://developer.hashicorp.com/terraform/language/meta-arguments/lifecycle
      + default, when Terraform must change a resource argument that cannot be updated in-place due to remote API limitations, Terraform will instead destroy the existing object and then create a new replacement object with the new configured arguments
  * https://developer.hashicorp.com/terraform/registry/private#terraform-cloud-private-registry
  * https://www.hashicorp.com/blog/deploying-terraform-enterprise-in-airgapped-environments !
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams#managing-workspace-access
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/policy-results#policy-evaluation-run-stages
  * https://developer.hashicorp.com/terraform/cloud-docs/run/states complex
  * https://developer.hashicorp.com/terraform/cloud-docs/run/states complex
  * https://developer.hashicorp.com/terraform/tutorials/state/refresh
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams#the-owners-team
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration
  * https://developer.hashicorp.com/terraform/registry/modules/verified
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement
  * https://www.hashicorp.com/blog/detecting-and-managing-drift-with-terraform !
  * https://developer.hashicorp.com/terraform/tutorials/state/resource-lifecycle#prevent-resource-deletion
  * https://developer.hashicorp.com/terraform/language/settings/backends/configuration#partial-configuration
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/outputs#redact-sensitive-outputs
  * https://developer.hashicorp.com/terraform/language/data-sources
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/data-sources
     + data source arguments,attributes
  * https://developer.hashicorp.com/terraform/cloud-docs/run/cli#remote-applies !
  * https://developer.hashicorp.com/terraform/tutorials/cloud/drift-and-opa
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/api-tokens#organization-api-tokens
  * https://developer.hashicorp.com/terraform/language/files/dependency-lock
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/provider-versioning#upgrade-the-aws-provider-version
  * https://developer.hashicorp.com/terraform/language/meta-arguments/lifecycle#ignore_changes
  * https://registry.terraform.io/providers/hashicorp/vault/latest/docs
    + https://www.youtube.com/watch?v=fOybhcbuxJ0 
    + [using the Terraform Test Framework](https://www.youtube.com/watch?v=4U2S6sXcuac)
  * https://developer.hashicorp.com/terraform/intro/core-workflow
  * https://www.hashicorp.com/blog/using-terraform-to-improve-infrastructure-security
  * https://developer.hashicorp.com/terraform/tutorials/cloud/cloud-migrate#migrate-the-state-file
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#remove-a-resource-from-state
  * https://developer.hashicorp.com/terraform/tutorials/0-13/cloud-login
  * https://developer.hashicorp.com/terraform/cli/config/config-file#credentials-1
  * https://www.hashicorp.com/blog/detecting-and-managing-drift-with-terraform#Summary
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel - reporting all violations
  * https://developer.hashicorp.com/sentinel/docs/concepts/policy-as-code
  * https://developer.hashicorp.com/sentinel/docs/concepts/enforcement-levels
  * https://developer.hashicorp.com/terraform/tutorials/policy/sentinel-policy
  * https://developer.hashicorp.com/terraform/tutorials/policy/sentinel-testing
  * [Sentinel Playground](https://play.sentinelproject.io)
  * [Introduction to Sentinel - problem Sentinel is designed to solve](https://www.youtube.com/watch?v=Vy8s7AAvU6g)
  * https://medium.com/slalom-technology/using-hashicorp-sentinel-to-implement-policy-as-code-within-your-terraform-provisioning-workflow-c89aac0ecc8
  
     + https://github.com/BruceCutler/sentinel-aws-policies
  * https://shadow-soft.com/content/using-terraform-sentinel-for-infrastructure-governance   
  * https://developer.hashicorp.com/sentinel/docs/configuration
  * https://developer.hashicorp.com/sentinel/docs/imports/strings

  * https://github.com/hashicorp/terraform-guides/tree/master/cloud-management-platform
  * [enforce policy as code for HashiCorp products](https://developer.hashicorp.com/sentinel?ajs_aid=1012049b-5d09-4ace-a41e-3985e6706b3b)
  * https://developer.hashicorp.com/sentinel/intro/what
  * [sentinel policy libraries](https://registry.terraform.io/browse/policies?ajs_aid=1012049b-5d09-4ace-a41e-3985e6706b3b&product_intent=terraform)
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/versions
     + HashiCorp Terraform __1.6__   was released on October 4, 2023. It includes a testing framework to help developers validate their code
     + [terraform test framework](https://www.youtube.com/watch?v=N73chhccmo8)

  * https://developer.hashicorp.com/terraform/cli/commands/test
  * https://developer.hashicorp.com/terraform/language/tests
      + Assertions within tests can reference any existing named values that are available to other custom conditions within the main Terraform configuration
   
   + https://developer.hashicorp.com/terraform/language/tests#assertions
   + https://developer.hashicorp.com/terraform/language/tests#modules
   + https://developer.hashicorp.com/terraform/language/tests#providers
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/test
  * https://github.com/Derek-Ashmore/terraform-testing-examples/blob/main/modules/vnet/test_suite.tftest.hcl 
   + complex, unclear
   + apparently tftest.hcl can access module internals from run scope
  * https://github.com/christosgalano/terraform-testing-example/blob/main/tests/integration.tftest.hcl 
  * https://registry.terraform.io/providers/hashicorp/http/latest/docs
    + https://registry.terraform.io/providers/hashicorp/http/latest/docs/data-sources/http#example-usage
    
  * https://developer.hashicorp.com/terraform/language/checks
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/checks
  * https://registry.terraform.io/providers/hashicorp/local/latest/docs
  * https://registry.terraform.io/providers/hashicorp/local/latest/docs/data-sources/file#example-usage
  * https://registry.terraform.io/providers/hashicorp/assert/latest/docs
  * https://registry.terraform.io/providers/hashicorp/tls/latest/docs
  * https://developer.hashicorp.com/terraform/tutorials?utm_source=tf_registry&utm_content=sidebar
  * https://registry.terraform.io/providers/hashicorp/assert/latest/docs/functions/http_client_error#terraform-test-example
  
#### Vertex AI Notebook
  * https://cloud.google.com/vertex-ai/docs/tutorials/terraform/terraform-create-user-managed-notebooks-instance
  * https://cloud.google.com/vertex-ai/docs/start/use-terraform-vertex-ai#terraform_resources_vertex_ai
  * https://cloud.google.com/vertex-ai/docs/general/developer-tools-overview
  * https://github.com/GoogleCloudPlatform/terraform-google-vertex-ai/blob/main/examples/workbench_simple_example/main.tf
  * https://github.com/GoogleCloudPlatform/terraform-google-vertex-ai/blob/main/modules/workbench/main.tf
  * https://github.com/GoogleCloudPlatform/terraform-google-vertex-ai/blob/main/examples/workbench_simple_example/network.tf
  * https://github.com/terraform-google-modules/terraform-google-network
  * https://github.com/terraform-google-modules/terraform-google-network/blob/master/main.tf
### Kubernetes

  * https://www.terraform.io/use-cases/manage-kubernetes
  * https://developer.hashicorp.com/terraform/tutorials/kubernetes/kubernetes-provider
### Modules Registy Search
  * https://registry.terraform.io/modules/terraform-google-modules/cloud-nat
  
    + https://registry.terraform.io/modules/terraform-google-modules/cloud-nat/google/latest
    + NOTE: not found when browsing namespace https://registry.terraform.io/search/modules?namespace=terraform-google-modules

   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/sql_database
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/sql_database_instance
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_global_address
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/service_networking_connection
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_instance_group_manager
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_instance_group
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_instance
   
     + https://stackoverflow.com/questions/63401480/how-to-create-gcp-instance-with-public-ip-with-terraform
     + https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_address.html#example-usage-instance-with-ip
     + https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_instance#access_config
     + https://github.com/LinkedInLearning/advanced-terraform-3099246
   * https://registry.terraform.io/modules/terraform-google-modules/service-accounts/google/latest 
   * https://registry.terraform.io/modules/terraform-google-modules/network/google/latest

   * https://registry.terraform.io/modules/terraform-google-modules/network/google/latest/submodules/firewall-rules (submodule)
   * https://registry.terraform.io/modules/terraform-google-modules/service-accounts/google/latest/examples/multiple_service_accounts
     + https://github.com/terraform-google-modules/terraform-google-service-accounts/blob/master/main.tf - for argument processing
     + [create service account in google cloud using terraform](https://www.youtube.com/watch?v=jNRm3lqSFKk)
     + https://github.com/Pruthvi360/google-cloud-services/blob/master/create-service-account/variables.tf
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam#google_service_account_iam_member
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_folder_iam#google_folder_iam_member
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam 
     + https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam#google_project_iam_member
     * [terraform-google-modules - iam](https://registry.terraform.io/modules/terraform-google-modules/iam/google/latest)
   * https://github.com/terraform-google-modules/terraform-google-iam
     * [terraform-google-modules - gke](https://registry.terraform.io/modules/terraform-google-modules/kubernetes-engine/google/latest)
       + https://github.com/terraform-google-modules/terraform-google-kubernetes-engine
     * [terraform-google-modules - vertex ai](https://registry.terraform.io/modules/GoogleCloudPlatform/vertex-ai/google/latest)
      + https://cloud.google.com/vertex-ai/docs/start/use-terraform-vertex-ai
      + https://github.com/GoogleCloudPlatform/terraform-google-vertex-ai
      + https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/workbench_instance#example-usage---workbench-instance-basic 
     * https://github.com/terraform-google-modules/terraform-docs-samples/tree/main/vertex_ai -  big collection of terraform resources
      
   * https://stackoverflow.com/questions/55400579/how-to-attach-custom-gcp-role-to-a-gcp-service-account-using-terraform
   * https://stackoverflow.com/questions/73703967/terraform-gcp-how-to-specify-the-same-role-for-several-users
   * https://stackoverflow.com/questions/63915353/what-is-the-meaning-of-authoritative-and-authoritative-for-gcp-iam-bindings
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam_custom_role.html#example-usage
      + https://stackoverflow.com/questions/49582977/gcp-custom-iam-role-creation-with-terraform
   * [deny policy example](https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/iam_deny_policy)
   
   * [principal access boundary example](https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/iam_access_boundary_policy)
   * [interactive](https://cloud.google.com/iam/docs/configuring-resource-based-access)
   * [full resource names ](https://cloud.google.com/iam/docs/full-resource-names)
   * https://cloud.google.com/iam/docs/configuring-resource-based-access

   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_organization_policy#example-usage
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_health_check#example-usage---health-check-http
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_instance_group
   * [Terraform resource samples on GCP](https://cloud.google.com/docs/terraform/samples)
   * [terraform on google cloud Validate policies guide](https://cloud.google.com/docs/terraform/policy-validation/validate-policies) (complex)

   * [Implementing IAM access control as code with HashiCorp Terraform](https://cloud.google.com/blog/topics/developers-practitioners/implementing-iam-access-control-code-hashicorp-terraform) (example missing)
      + `Service Account User` grants access for a user (referenced as member) to assume a service account (service_account_id)  by granting the user the iam.ServiceAccountUser role (referenced as role above).
      + [Constraints](https://cloud.google.com/resource-manager/docs/organization-policy/understanding-constraints#available_constraints)
      * [IAM Intro](https://cloud-dot-devsite-v2-prod.appspot.com/iam/docs/grant-role-console)
   * [OpeN policy Agent](https://www.openpolicyagent.org/docs/latest/terraform)(in Golang) 
   * [Rego policy language](https://www.openpolicyagent.org/docs/latest/policy-language/)
   * https://cloud.google.com/docs/terraform/policy-validation/create-policy-library
   * https://cloud.google.com/docs/terraform/samples
     + https://github.com/terraform-google-modules/terraform-docs-samples/tree/main/iam/create_deny_policy
   * [kitchen-terraform](https://newcontext-oss.github.io/kitchen-terraform/getting_started.html)
   * https://registry.terraform.io/providers/hashicorp/google/4.75.0/docs/resources/google_folder_iam
   * [workload Identity in GKE with Terraform](https://surajblog.medium.com/workload-identity-in-gke-with-terraform-9678a7a1d9c0) 
      + allowing Kubernetes workloads to authenticate as Google Cloud service accounts
   * https://cloud.google.com/vertex-ai/docs/start/use-terraform-vertex-ai 
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_firewall
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_network
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_subnetwork
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_subnetwork#example-usage
     + (does not appear to be useful?)
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_disk
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_attached_disk
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_service#example-usage
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project#example-usage
   * https://registry.terraform.io/modules/terraform-google-modules/network/google/latest
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_instance
   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/namespace
   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/replication_controller
     + https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/replication_controller#example-usage
   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/deployment#example-usage
   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/service#example-usage

   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/resource_group
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_account
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_account#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_container
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_container#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/subnet
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/ 
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_machine
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/data-sources/virtual_network
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/data-sources/virtual_network#subnets
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service_plan
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service.html#linux_fx_version
   * https://registry.terraform.io/providers/hashicorp/template/latest/docs/data-sources/file.html  
   * [Sentinel for Terraform Part 1](https://www.youtube.com/watch?v=YZfA-TqkcV0)
   * [Sentinel for Terraform Part 2](https://www.youtube.com/watch?v=DmlThcAAeIs)
    
### Misc


  * https://www.hashicorp.com/blog/testing-hashicorp-terraform
  * https://github.com/hashicorp-education/learn-terraform-test 
 
  * https://github.com/in28minutes/course-material
  * https://terratest.gruntwork.io
     + https://github.com/gruntwork-io/terratest
     + https://github.com/gruntwork-io/terratest/tree/master/modules/gcp
     + https://github.com/philjhale/terratest-gcp
     + https://github.com/retpolanne/gcp-terratest
     + https://github.com/srikantharun/terratest-on-gcp
     + https://github.com/kaushikgayal/terratest-gcp (pure Terraform)
     + https://github.com/sthirion/terratest-your-terraform
     + https://github.com/akash123-eng/terratest-sample
     + https://github.com/krismorte/terraform-and-terratest-example
     + https://github.com/martinbaillie/terratest-istio
     + https://github.com/kuntrapakam/sample_terratest
     + https://github.com/rujwal-shrestha/generate-base-terratest/blob/main/genterratest.sh
     + https://github.com/bradib0y/terratest-demox
     + https://github.com/denis256/terratest-tests
  * https://github.com/gruntwork-io/terratest/blob/master/examples/terraform-gcp-example     
  * https://docs.terrakube.io/
  * https://terrakube.org/
  * https://www.hashicorp.com/blog/testing-hashicorp-terraform
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/test
  * [test Terraform Code – Strategies & Tools](https://spacelift.io/blog/terraform-test)
  * https://developer.hashicorp.com/terraform/language/tests
  * [comprehensive Look at 14 Popular Terraform Testing Tools](https://zeet.co/blog/terraform-testing-tools)
  * [comprehensive Guide to Testing in Terraform: Keep your tests, validations, checks, and policies in order](https://mattias.engineer/blog/2023/terraform-testing-and-validation)
  *   
  * [terraform Test Framework](https://tf2project.io/index.html)
  * https://www.reddit.com/r/devops/comments/13g3vug/testing_terraform_code
  * https://medium.com/@monusraj/using-terraform-test-for-testing-your-terraform-code-b6fdbe170ba0 (too short)
  * https://developer.hashicorp.com/terraform/cli/test
  * https://mattias.engineer/blog/2023/terraform-testing-deep-dive (complex, many detals)
  * https://docs.aws.amazon.com/prescriptive-guidance/latest/patterns/use-serverspec-for-test-driven-development-of-infrastructure-code.html
  * https://blog.unif.io/test-driven-development-of-infrastructure-code-9146d3d6c780
  * https://medium.com/sumup-engineering/image-creation-and-testing-with-hashicorp-packer-and-serverspec-bb2bd065441
  * https://faun.pub/terraform-code-testing-61ff5ea4b68a
  * https://stackoverflow.com/questions/37844113/integrating-terraform-and-serverspec
  * https://thepracticalsysadmin.com/terraform-testing-tools/
  * https://abstraction.blog/2021/06/20/terraform-testing-tools-comparison
  * https://developer.hashicorp.com/terraform/tutorials/state/resource-targeting - complex
  * [How to Create GKE Cluster Using TERRAFORM](https://www.youtube.com/watch?v=X_IK0GBbBTw) (complex)
  * [Terraform Tutorials for Beginners](https://www.youtube.com/playlist?list=PLiMWaCMwGJXmJdmfJjG3aK1IkU7oWvxIj)
    + [Loops Conditionals](https://youtu.be/7S94oUTy2z4)
    + [lessons](https://github.com/antonputra/tutorials/blob/main/docs/contents.md)
    + https://github.com/antonputra/tutorials/tree/main/lessons/161
    + https://cidr.xyz/
    +  https://www.youtube.com/watch?v=7S94oUTy2z4&list=PLiMWaCMwGJXmJdmfJjG3aK1IkU7oWvxIj&index=4)
  * https://developer.hashicorp.com/terraform/language/expressions/strings#directives     
  * [building complex templates by string template interpolation](https://medium.com/ovni/terraform-templating-and-loops-9a88c0786c5c)
  * https://registry.terraform.io/providers/
  * [serverspec with terraform](https://www.contino.io/insights/top-3-terraform-testing-strategies-for-ultra-reliable-infrastructure-as-code) -
  
### Terraform GCP  
  * https://github.com/steinim/gcp-terraform-workshop (old code)
  * https://www.udemy.com/course/mastering-terraform-on-google-cloud-platform-gcp/?ysclid=m0pivxvwli791541030
  * https://sanet.st/blogs/bonnytuts/mastering_terraform_on_google_cloud_platform_gcp.4544191.html
  * https://www.udemy.com/course/mastering-terraform-on-google-cloud-platform-gcp/
  * https://cloudfoundation.com/terraform-gcp-training/
  * https://github.com/iamashok1410/terraform-course-material 
  * https://jayan-menon.medium.com/prepare-for-terraform-associate-exam-with-gcp-part-1-7657015c11cf
  * https://sanet.st/blogs/exclusivetutorials/terraform_for_beginners_using_gcp_google_cloud_hands_on.3900106.html
  * https://cloud.google.com/docs/terraform
   + https://cloud.google.com/docs/terraform/deploy-flask-web-server
   + https://cloud.google.com/docs/terraform/resource-management/export
   
   
  * https://terraform-docs-common.vercel.app/terraform/tutorials/gcp-get-started/google-cloud-platform-change
  * https://cloud.google.com/docs/terraform/samples
  * https://medium.com/slalom-technology/a-complete-gcp-environment-with-terraform-c087190366f0 (-incomplete)
  
  * https://github.com/broisnischal/gcp-terraform/tree/master/infrastructure (cryptic)
  * https://github.com/hashicorp/learn-terraform-provision-gke-cluster
  * https://console.cloud.google.com/gcr/images/google-samples/global/hello-app  
  
  * https://github.com/liptun/gcp-test
    + https://github.com/liptun/gcp-test/blob/master/terraform/cloud-run.tf
    
  * [Creating GCP Service Account in Console with IAM Role conditions](https://www.youtube.com/watch?v=u_S1RWNKiNU)  
  * [Creating GCP Service Account in Terraform with IAM Role conditions](https://www.youtube.com/watch?v=nRpgZfAgXQc)
    + https://cloud.google.com/certificate-authority-service/docs/using-cel#iam-policies
    + https://cloud.google.com/iam/docs/configuring-temporary-access#iam-conditions-expirable-access-gcloud
  * https://github.com/terraform-google-modules/terraform-google-kubernetes-engine
  
  * https://github.com/indrajitp/terraform-gcp-custom-roles/blob/master/modules/custom_role_iam/main.tf
   + permissions from predefined roles
  * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_role 
  
  * [Terraform Test Framework | Terraform unit testing | Terraform Contract test | terraform testing](https://www.youtube.com/watch?v=oHW7dKBGwX8)
  * https://www.hashicorp.com/blog/testing-hashicorp-terraform
  * https://www.hashicorp.com/blog/terraform-1-7-adds-test-mocking-and-config-driven-remove
  * [terratest](https://medium.com/@mohsenny/terraform-testing-a-qa-engineers-playbook-35385e147d88)
  * [terraform to Create GKE cluster in Google Cloud](https://www.youtube.com/watch?v=JAjTTQEwO04)
  * https://spacelift.io/blog/importing-exisiting-infrastructure-into-terraform
  * https://www.cloudbolt.io/terraform-best-practices/terraform-import-example
  * https://controlmonkey.io/blog/the-ultimate-devops-guide-to-terraform-import
  
  * https://github.com/gruntwork-io/terraform-google-static-assets/blob/4d2f6df1f07837437b00f892a5311cdfb6a5374a/modules/cloud-storage-static-website/main.tf
  * https://medium.com/swlh/setup-a-static-website-cdn-with-terraform-on-gcp-23c6937382c6
  * https://cloud.google.com/storage/docs/hosting-static-website
  * https://cloud.google.com/storage/docs/hosting-static-website  
  `storage.googleapis.com/[BUCKET_NAME]/[OBJECT_NAME]`
  
  * [Kubernetes Tutorials](https://www.youtube.com/playlist?list=PLiMWaCMwGJXnHmccp2xlBENZ1xr4FpjXF) (complex)
  * https://sanet.st/blogs/mecury-books/terraform_for_google_cloud_essential_guide_learn_how_to_provision_infrastructure_in_google_cloud_securely_and_efficiently.4347394.html
   *  https://stackoverflow.com/questions/49392448/possible-to-host-a-website-with-google-cloud-without-a-domain
  * https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide
  * https://github.com/PacktPublishing/Terraform-for-Google-Cloud-Essential-Guide/blob/main/chap03
  * https://cloud.google.com/firewall/docs/using-firewalls#rules-for-common-use-cases
  * https://registry.terraform.io/providers/hashicorp/google/3.32.0/docs/data-sources/netblock_ip_ranges
  * https://stackoverflow.com/questions/38645002/how-to-add-an-ssh-key-to-an-gcp-instance-using-terraform
  * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_project_metadata#argument-reference
  * https://medium.com/google-cloud/terraform-remote-exec-on-google-compute-engine-vm-instance-d47def447072
  * https://github.com/Sayed-Imran/Terraform-Scripts/blob/master/gcp-remote-exec
  * https://gist.github.com/smford22/54aa5e96701430f1bb0ea6e1a502d23a
  * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_project_metadata
  * https://kodekloud.com/community/t/how-to-login-ssh-into-gcp-machine-using-private-key-in-terraform/25392
  *   
  * https://developer.hashicorp.com/terraform/cli/state/resource-addressing
  * https://dockerhosting.ru/blog/28-%D0%B2%D0%BE%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B2-%D0%B8-%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D0%BE%D0%B2-%D0%BD%D0%B0-%D1%81%D0%BE%D0%B1%D0%B5%D1%81%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B8-%D0%BF/#%E2%84%96_10_%D0%A7%D1%82%D0%BE_%D1%82%D0%B0%D0%BA%D0%BE%D0%B5_%D0%A1%D0%B5%D0%BD%D1%82%D0%B8%D0%BD%D0%B5%D0%BB
  
  * https://cloud.google.com/blog/topics/developers-practitioners/using-google-cloud-service-account-impersonation-your-terraform-code
  * https://medium.com/bluetuple-ai/terraform-remote-state-on-gcp-d50e2f69b967
    + https://github.com/bluetuple/terraform-gcp/tree/main/account-impersonation
  * [null resource provisioners](https://www.devopsschool.com/blog/terrafrom-example-code-for-remote-exec-provisioner)
  * https://www.redhat.com/sysadmin/route-ip-route
  * https://www.cyberciti.biz/faq/what-is-a-routing-table/
  * https://registry.terraform.io/providers/hashicorp/google/latest/docs/guides/provider_reference.html
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/for-each

#### Go
  *   https://github.com/Ebazhanov/linkedin-skill-assessments-quizzes/blob/main/go/go-quiz.md?ysclid=m1d06h5hpd677110313
  
 #### Misc 
 
    * https://community.t-mobile.com/troubleshooting-38/unable-to-use-t-mobile-home-internet-with-work-vpn-any-suggestions-52100
    * https://community.t-mobile.com/troubleshooting-38/globalprotect-vpn-not-working-with-t-mobile-35992
    * https://youtu.be/e9YavAW09hI?t=162
      + `netsh interface ipv4 show subinterfaces`
       + `netsh interface ipv4 set subinterface "Wi-Fi" mtu=1478 store=persistent`
  * https://stackoverflow.com/questions/53162620/automate-gcp-persistent-disk-initialization?rq=4
  * https://github.com/terraform-google-modules/terraform-google-network/tree/master/examples
  * https://medium.com/@4get.prakhar/google-cloud-iam-policies-69dc027d21a
  * https://www.blinkops.com/blog/managing-iam-policies-with-the-google-cloud-cli
  * https://stackoverflow.com/questions/66016313/how-to-get-json-representation-from-search-all-iam-policies-results
  * [How to use short-lived credentials to authorize Terraform with GCP instead of service account keys](https://www.youtube.com/watch?v=UaRzdTTq9O4)  via env:GOOGLE_OAUTH_ACCESS_TOKEN
  * https://registry.terraform.io/providers/hashicorp/google/latest/docs/guides/provider_reference#authentication-configuration
  
#### HCP VCS
  * https://github-app-tutorial.readthedocs.io/en/latest/creating-github-app.html

  * https://developer.hashicorp.com/terraform/cloud-docs/workspaces/settings/vcs  
  * https://developer.hashicorp.com/terraform/cloud-docs/vcs/github-app
  * [sentinel  through HCP](https://www.youtube.com/watch?v=7ohfIsnEVgU&list=PLBl7DXGGh5zgCvbngCNnG--tJup_HxRfL&index=12)
  
 ### Data Sources
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_role - returns included_permissions
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/folder_iam_policy returns policy_data
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/iam_policy
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/folder_organization_policy
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/service_account_iam_policy
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_instance
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_instance_group
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_instance_group_manager
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_health_check
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_subnetwork
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/compute_subnetworks
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/container_cluster 
 * https://registry.terraform.io/providers/hashicorp/google/latest/docs/data-sources/service_account_access_token

### Gloud  
  * https://github.com/terraform-google-modules/terraform-google-gcloud/README.md - complex 
  * https://registry.terraform.io/modules/terraform-google-modules/gcloud/google/latest
  * https://stackoverflow.com/questions/75452076/gcp-output-csv-file-using-local-exec-in-terraform
  * https://stackoverflow.com/questions/57454591/how-can-i-load-input-data-from-a-file-in-terraform
  * https://www.reddit.com/r/Terraform/comments/xv39l0/output_of_localexec_to_variable/
  * https://discuss.hashicorp.com/t/how-to-retrieve-the-null-resource-returned-value/9620/3
  * https://registry.terraform.io/providers/hashicorp/external/latest/docs/data-sources/external
  * https://registry.terraform.io/providers/hashicorp/local/latest/docs/data-sources/file
  * [external data source example](https://gist.github.com/irvingpop/968464132ded25a206ced835d50afa6b)
  
### Inspec GCP Tests
   
  * [Best practices for testing](https://cloud.google.com/docs/terraform/best-practices/testing)
  * [InSpec GCP project](https://github.com/inspec/inspec-gcp)
  * Linked documentation of inspec controls:
     + https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_service_account.md
  * [Google Cloud Platform support for InSpec](https://lollyrock.com/posts/inspec-cloud-gcp-setup/)
  * [InSpec GCP Deep Dive](https://www.chef.io/blog/inspec-gcp-deep-dive)
    + https://github.com/skpaterson/inspec-gcp-deep-dive-profile
    + NOTE: may need to include terraform code in the fixture directory e.g.
      - 
    + NOTE:  the notation:`url: https://github.com/inspec/inspec-gcp/archive/master.tar.gz`
    * [Chef InSpec Tutorials](https://origin.inspec.io/tutorials/)
  * https://lollyrock.com/posts/inspec-terraform/
    + https://github.com/chris-rock/testing-4-cloud/tree/master/gcp-example-profile
  * https://github.com/terraform-google-modules/terraform-google-iam/blob/master/test/integration/authoritative/controls/authoritative.rb
  * https://docs.chef.io/inspec/cli/
  * https://www.chef.io/blog/understanding-singular-and-plural-inspec-resources
  * https://docs.chef.io/inspec/install/ - installer is difficult to find
  * [Windows Infrastructure Testing and Compliance with InSpec](https://lollyrock.com/posts/inspec-windows/)
  * https://docs.chef.io/inspec/install/#installer
  * https://www.rearc.io/blog/testing-terraform-with-kitchen-and-inspec
  * https://github.com/terraform-google-modules/terraform-google-service-accounts/blob/master/main.tf - for argument processing
  * https://github.com/terraform-google-modules/terraform-google-iam
  * Free Tier  license key : `free-f47594c0-4e89-4f8a-a036-a40d5ce2c825-2747`
  * [Use iggy to generate InSpec controls based on the terraform state file](https://www.youtube.com/watch?v=-PBq_qkTX5Q&pp=ygUKaW5zcGVjIGdjcA%3D%3D)
  * https://www.youtube.com/watch?v=L8ZHcCNFko8 - complex
  * [Generating InSpec coverage from Terraform with InSpec-Iggy](https://www.youtube.com/watch?v=4AJ8MS4BaQ0&pp=ygUKaW5zcGVjIGdjcA%3D%3D)
  * https://www.youtube.com/watch?v=aeN_jZezhS8&pp=ygUKaW5zcGVjIGdjcA%3D%3D
  * [Introduction to Test Kitchen - Google Cloud Platform](https://www.youtube.com/watch?v=HDOMXmp14es&list=PLKK5zTDXqzFMb-d7_K12W3ZwujfPvoDiK)
  * https://docs.chef.io/inspec/#:~:text=Chef%20InSpec%20has%201189%20resources,resource%20that%20meets%20your%20needs.
  * https://docs.chef.io/inspec/cloud/gcp/
  * https://www.chef.io/blog/did-you-know-this-about-chef-compliance-and-chef-cloud-security-part-3
  * https://www.chef.io/blog/making-use-of-google-inspec-cloud-resource