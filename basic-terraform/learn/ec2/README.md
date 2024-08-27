### Info
```cmd	
terraform plan -out plan.out
```

```cmd	
terraform show  -json plan.out >a.json
```
```cmd
type a.json | c:\tools\jq-win64.exe "." > a1.json
```
`a1.json`:	
```json
{
  "format_version": "1.2",
  "terraform_version": "1.7.5",
  "variables": {
    "cidr_blocks": {
      "value": "202.173.127.130/32"
    },
    "instance_name": {
      "value": "terratest-example"
    }
  },
  "planned_values": {
    "root_module": {
      "resources": [
        {
          "address": "aws_instance.ec2",
          "mode": "managed",
          "type": "aws_instance",
          "name": "ec2",
          "provider_name": "registry.terraform.io/hashicorp/aws",
          "schema_version": 1,
          "values": {
            "ami": "ami-0a0277ba899dd9fd3",
            "credit_specification": [],
            "get_password_data": false,
            "hibernation": null,
            "instance_type": "t2.micro",
            "launch_template": [],
            "source_dest_check": true,
            "tags": {
              "Name": "terratest-example"
            },
            "tags_all": {
              "Name": "terratest-example"
n            },
            "timeouts": null,
            "user_data_replace_on_change": false,
            "volume_tags": null
          },
          "sensitive_values": {
            "capacity_reservation_specification": [],
            "cpu_options": [],
            "credit_specification": [],
            "ebs_block_device": [],
            "enclave_options": [],
            "ephemeral_block_device": [],
            "instance_market_options": [],
            "ipv6_addresses": [],
            "launch_template": [],
            "maintenance_options": [],
            "metadata_options": [],
            "network_interface": [],
            "private_dns_name_options": [],
            "root_block_device": [],
            "secondary_private_ips": [],
            "security_groups": [],
            "tags": {},
            "tags_all": {},
            "vpc_security_group_ids": []
          }
        }
      ]
    }
  },
  "resource_changes": [
    {
      "address": "aws_instance.ec2",
      "mode": "managed",
      "type": "aws_instance",
      "name": "ec2",
      "provider_name": "registry.terraform.io/hashicorp/aws",
      "change": {
        "actions": [
          "create"
        ],
        "before": null,
        "after": {
          "ami": "ami-0a0277ba899dd9fd3",
          "credit_specification": [],
          "get_password_data": false,
          "hibernation": null,
          "instance_type": "t2.micro",
          "launch_template": [],
          "source_dest_check": true,
          "tags": {
            "Name": "terratest-example"
          },
          "tags_all": {
            "Name": "terratest-example"
          },
          "timeouts": null,
          "user_data_replace_on_change": false,
          "volume_tags": null
        },
        "after_unknown": {
          "arn": true,
          "associate_public_ip_address": true,
          "availability_zone": true,
          "capacity_reservation_specification": true,
          "cpu_core_count": true,
          "cpu_options": true,
          "cpu_threads_per_core": true,
          "credit_specification": [],
          "disable_api_stop": true,
          "disable_api_termination": true,
          "ebs_block_device": true,
          "ebs_optimized": true,
          "enclave_options": true,
          "ephemeral_block_device": true,
          "host_id": true,
          "host_resource_group_arn": true,
          "iam_instance_profile": true,
          "id": true,
          "instance_initiated_shutdown_behavior": true,
          "instance_lifecycle": true,
          "instance_market_options": true,
          "instance_state": true,
          "ipv6_address_count": true,
          "ipv6_addresses": true,
          "key_name": true,
          "launch_template": [],
          "maintenance_options": true,
          "metadata_options": true,
          "monitoring": true,
          "network_interface": true,
          "outpost_arn": true,
          "password_data": true,
          "placement_group": true,
          "placement_partition_number": true,
          "primary_network_interface_id": true,
          "private_dns": true,
          "private_dns_name_options": true,
          "private_ip": true,
          "public_dns": true,
          "public_ip": true,
          "root_block_device": true,
          "secondary_private_ips": true,
          "security_groups": true,
          "spot_instance_request_id": true,
          "subnet_id": true,
          "tags": {},
          "tags_all": {},
          "tenancy": true,
          "user_data": true,
          "user_data_base64": true,
          "vpc_security_group_ids": true
        },
        "before_sensitive": false,
        "after_sensitive": {
          "capacity_reservation_specification": [],
          "cpu_options": [],
          "credit_specification": [],
          "ebs_block_device": [],
          "enclave_options": [],
          "ephemeral_block_device": [],
          "instance_market_options": [],
          "ipv6_addresses": [],
          "launch_template": [],
          "maintenance_options": [],
          "metadata_options": [],
          "network_interface": [],
          "private_dns_name_options": [],
          "root_block_device": [],
          "secondary_private_ips": [],
          "security_groups": [],
          "tags": {},
          "tags_all": {},
          "vpc_security_group_ids": []
        }
      }
    }
  ],
  "configuration": {
    "provider_config": {
      "aws": {
        "name": "aws",
        "full_name": "registry.terraform.io/hashicorp/aws",
        "expressions": {
          "access_key": {
            "constant_value": "ACCESS_KEY"
          },
          "region": {
            "constant_value": "us-east-2"
          },
          "secret_key": {
            "constant_value": "SECRET_KEY"
          }
        }
      }
    },
    "root_module": {
      "resources": [
        {
          "address": "aws_instance.ec2",
          "mode": "managed",
          "type": "aws_instance",
          "name": "ec2",
          "provider_config_key": "aws",
          "expressions": {
            "ami": {
              "constant_value": "ami-0a0277ba899dd9fd3"
            },
            "instance_type": {
              "constant_value": "t2.micro"
            },
            "tags": {
              "references": [
                "var.instance_name"
              ]
            }
          },
          "schema_version": 1
        }
      ],
      "variables": {
        "cidr_blocks": {},
        "instance_name": {
          "default": "terratest-example",
          "description": "The Name tag to set for the EC2 Instance."
        }
      }
    }
  },
  "timestamp": "2024-08-27T01:12:31Z",
  "errored": false
}

```

```cmd

type a.json | c:\tools\jq-win64.exe ".planned_values.root_module.resources[0].values.tags"
```
```json
{
  "Name": "terratest-example"
}
```

Compare with [terraform_aws_example_plan_test.go](https://github.com/gruntwork-io/terratest/blob/master/test/terraform_aws_example_plan_test.go):
```go

var jsonEC2Tags []map[string]interface{}
	jsonOut := terraform.InitAndPlanAndShow(t, terraformOptions)
	k8s.UnmarshalJSONPath(
		t,
		[]byte(jsonOut),
		"{ .planned_values.root_module.resources[0].values.tags }",
		&jsonEC2Tags,
	)
	assert.Equal(t, map[string]interface{}{"Name": expectedName}, jsonEC2Tags[0])
```
