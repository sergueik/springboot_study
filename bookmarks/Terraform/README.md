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
  * https://developer.hashicorp.com/sentinel/docs/language/spec#else-operator
  * https://developer.hashicorp.com/sentinel/docs/terraform#examples HCP Terraform evaluates Sentinel policy evaluations immediately before cost estimation
  * https://developer.hashicorp.com/sentinel/docs/language
   + https://github.com/hashicorp/terraform-sentinel-policies/tree/main/gcp
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
  * https://developer.hashicorp.com/terraform/cli/init#reinitializing-only-modules
     + get
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
  * https://developer.hashicorp.com/terraform/language/functions/regex
  * https://developer.hashicorp.com/terraform/language/functions/list
  * https://developer.hashicorp.com/terraform/language/functions/tostring#examples
  * https://developer.hashicorp.com/terraform/language/functions/setproduct
  * https://developer.hashicorp.com/terraform/language/functions/setintersection
  * https://developer.hashicorp.com/terraform/language/functions/setsubtract
  * https://developer.hashicorp.com/terraform/language/functions/setunion
  * https://developer.hashicorp.com/terraform/language/functions/transpose
  * https://developer.hashicorp.com/terraform/language/functions/zipmap
  * https://www.terraform.io/docs/configuration/functions/element.html
     - index greater than the length of the list is "wrapped around" modulo the length of the list
  * https://developer.hashicorp.com/terraform/language/functions/can
  * https://developer.hashicorp.com/terraform/language/functions/cidrhost
  * https://developer.hashicorp.com/terraform/language/functions/try
  * https://developer.hashicorp.com/terraform/language/functions/yamldecode
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
    + https://www.youtube.com/watch?v=hqCHQtXLpiI
  * https://developer.hashicorp.com/terraform/cloud-docs/users-teams-organizations/teams#managing-workspace-access
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/policy-results#policy-evaluation-run-stages
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
  * https://developer.hashicorp.com/terraform/intro/core-workflow
  * https://www.hashicorp.com/blog/using-terraform-to-improve-infrastructure-security
  * https://developer.hashicorp.com/terraform/tutorials/cloud/cloud-migrate#migrate-the-state-file
  * https://developer.hashicorp.com/terraform/tutorials/state/state-cli#remove-a-resource-from-state
  * https://developer.hashicorp.com/terraform/tutorials/0-13/cloud-login
  * https://developer.hashicorp.com/terraform/cli/config/config-file#credentials-1
  * https://www.hashicorp.com/blog/detecting-and-managing-drift-with-terraform#Summary
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/sentinel
  * https://developer.hashicorp.com/terraform/cloud-docs/policy-enforcement/opa#example-policies
  * https://developer.hashicorp.com/sentinel/docs/concepts/policy-as-code
  * https://developer.hashicorp.com/sentinel/docs/concepts/enforcement-levels
  * https://developer.hashicorp.com/terraform/tutorials/policy/sentinel-policy
  * https://developer.hashicorp.com/terraform/tutorials/policy/sentinel-testing
  * [Sentinel Playground](https://play.sentinelproject.io)
  * [Introduction to Sentinel - problem Sentinel is designed to solve](https://www.youtube.com/watch?v=Vy8s7AAvU6g)
  * https://medium.com/slalom-technology/using-hashicorp-sentinel-to-implement-policy-as-code-within-your-terraform-provisioning-workflow-c89aac0ecc8
     + https://github.com/BruceCutler/sentinel-aws-policies
  * https://github.com/hashicorp/terraform-guides/tree/master/cloud-management-platform
  * [enforce policy as code for HashiCorp products](https://developer.hashicorp.com/sentinel?ajs_aid=1012049b-5d09-4ace-a41e-3985e6706b3b)
  * https://developer.hashicorp.com/sentinel/intro/what
  * [sentinel policy libraries](https://registry.terraform.io/browse/policies?ajs_aid=1012049b-5d09-4ace-a41e-3985e6706b3b&product_intent=terraform)
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/versions
  * https://developer.hashicorp.com/terraform/cli/test
     + HashiCorp Terraform __1.6__   was released on October 4, 2023. It includes a testing framework to help developers validate their code
     + https://www.youtube.com/watch?v=N73chhccmo8
  * https://developer.hashicorp.com/terraform/cli/commands/test
  * https://developer.hashicorp.com/terraform/language/tests
   + https://developer.hashicorp.com/terraform/language/tests#assertions
   + https://developer.hashicorp.com/terraform/language/tests#modules
   + https://developer.hashicorp.com/terraform/language/tests#providers
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/test
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
     + https://github.com/terraform-google-modules/terraform-google-service-accounts/blob/master/main.tf - for argument processing
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam#google_service_account_iam_member   
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam 
     + https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_project_iam#google_project_iam_member
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account_iam
   * https://registry.terraform.io/providers/hashicorp/google/4.75.0/docs/resources/google_folder_iam
   
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_firewall
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_network
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_subnetwork
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_disk
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/compute_attached_disk
   * https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/google_service_account

   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/namespace
   * https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/replication_controller
     + https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/replication_controller#example-usage
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/resource_group
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_account
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_account#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_container
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_container#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/subnet
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/network_interface
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_machine
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/virtual_network#attributes-reference
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/data-sources/virtual_network
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/data-sources/virtual_network#subnets
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service_plan
   * https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service
     + https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/app_service.html#linux_fx_version
     
### Misc

  * https://www.hashicorp.com/blog/testing-hashicorp-terraform
  * https://github.com/hashicorp-education/learn-terraform-test 
 
  * https://github.com/in28minutes/course-material
  * https://terratest.gruntwork.io
     + https://github.com/gruntwork-io/terratest
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
     + https://github.com/bradib0y/terratest-demo
     + https://github.com/denis256/terratest-tests
  * https://docs.terrakube.io/
  * https://terrakube.org/
  * [terraform test framework](https://www.youtube.com/watch?v=N73chhccmo8)
  * https://www.hashicorp.com/blog/testing-hashicorp-terraform
  * https://developer.hashicorp.com/terraform/tutorials/configuration-language/test
  * [test Terraform Code – Strategies & Tools](https://spacelift.io/blog/terraform-test)
  * https://developer.hashicorp.com/terraform/language/tests
  * [comprehensive Look at 14 Popular Terraform Testing Tools](https://zeet.co/blog/terraform-testing-tools)
  * [comprehensive Guide to Testing in Terraform: Keep your tests, validations, checks, and policies in order](https://mattias.engineer/blog/2023/terraform-testing-and-validation/)
  * https://learn.microsoft.com/en-us/azure/developer/terraform/best-practices-testing-overview
  * [terraform Test Framework](https://tf2project.io/index.html)
  * https://www.reddit.com/r/devops/comments/13g3vug/testing_terraform_code/
  * https://medium.com/@monusraj/using-terraform-test-for-testing-your-terraform-code-b6fdbe170ba0
  * https://developer.hashicorp.com/terraform/cli/test
  * https://mattias.engineer/blog/2023/terraform-testing-deep-dive
  * [terraform Test Framework](https://www.youtube.com/watch?v=oHW7dKBGwX8)
  * https://docs.aws.amazon.com/prescriptive-guidance/latest/patterns/use-serverspec-for-test-driven-development-of-infrastructure-code.html
  * https://blog.unif.io/test-driven-development-of-infrastructure-code-9146d3d6c780
  * https://medium.com/sumup-engineering/image-creation-and-testing-with-hashicorp-packer-and-serverspec-bb2bd065441
  * https://faun.pub/terraform-code-testing-61ff5ea4b68a
  * https://stackoverflow.com/questions/37844113/integrating-terraform-and-serverspec
  * https://thepracticalsysadmin.com/terraform-testing-tools/
  * https://abstraction.blog/2021/06/20/terraform-testing-tools-comparison
  * https://devops.stackexchange.com/questions/863/how-to-test-a-terraform-configuration
  * https://lollyrock.com/posts/inspec-terraform/
  * [How to Create GKE Cluster Using TERRAFORM](https://www.youtube.com/watch?v=X_IK0GBbBTw)(complex)
  * https://registry.terraform.io/providers/
