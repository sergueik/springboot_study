### Info

https://registry.terraform.io/providers/terra-farm/virtualbox/latest/docs/resources/vm


### Install

```sh
export VERSION=1.1.2
wget -q https://releases.hashicorp.com/terraform/$VERSION/terraform_${VERSION}_linux_amd64.zip
unzip terraform_${VERSION}_linux_amd64.zip
sudo mv terraform /usr/local/bin/
```
```sh
terraform -version
```

```text
Terraform v1.1.5
on linux_amd64
```

* update the version of the required provider checking the available on [registry](https://registry.terraform.io/providers/terra-farm/virtualbox/latest/docs):
```text
  required_providers {
    virtualbox = {
      source = "terra-farm/virtualbox"
      version = "0.2.2-alpha.1"
    }
  }

```
(no automated way learned yet)

### Testing

* comment the non-existent user_data reference

```text
user_data = file("${path.module}/user_data")
```
* rerun init
```sh
terraform init
```
* plan saving the state
```sh
terraform plan -out a.out
```
```sh
export TF_LOG=info
terraform apply virtualbox.out
```

this will fail with the version 0.22 of the virtualbox provider with
```text
Error: [ERROR] Unable to fetch remote image: unsupported scheme file
```
* modify the configuration with the download link information from `https://app.vagrantup.com/generic/boxes/alpine39`:
```text
# image = "file:///home/sergueik/Downloads/alpine39.box"
image = "https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box"
```
update the network configuration as available locally in Virtual Box:
```text
 network_adapter {
    type           = "hostonly"
    host_interface = "vboxnet0"
  }
```
and re-aplly
```sh
terraform apply a.out
```
NOTE: it will take considerable time on a moderately powered machine and end up with
```text
virtualbox_vm.node[0]: Creating...
virtualbox_vm.node[1]: Creating...
virtualbox_vm.node[1]: Still creating... [10s elapsed]
virtualbox_vm.node[0]: Still creating... [10s elapsed]
...
virtualbox_vm.node[1]: Creation complete after 1m23s [id=f4790fae-8e6f-410a-be89-b103c7f41d8e]
virtualbox_vm.node[0]: Still creating... [1m30s elapsed]
virtualbox_vm.node[0]: Still creating... [1m40s elapsed]
virtualbox_vm.node[0]: Creation complete after 1m46s [id=2465f3d7-08fa-4f05-9340-0b0a98d04b17]
```

if a log was set fo info, there will be some more low level information:

```text
2022-02-11T17:01:36.843-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=60%...70%...80%...100%
2022-02-11T17:01:36.893-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stdout="Clone medium created in format 'VMDK'. UUID: 87afba8f-66a1-4593-bdd8-f522664e0387"
2022-02-11T17:01:57.275-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stdout="Waiting for VM "node-02" to power on...VM "node-02" has been successfully started."
```
### Cleanup
```sh
unset TF_LOG
terraform plan -destroy -out a.out
terraform apply a.out
```
### See Also

* [install Terraform on Ubuntu Bionic 18.04 Server](https://www.decodingdevops.com/how-to-install-terraform-on-ubuntu-18-04-server/)
