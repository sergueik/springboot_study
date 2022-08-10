### Info

https://registry.terraform.io/providers/terra-farm/virtualbox/latest/docs/resources/vm


### Install

```sh
export VERSION=1.1.2
wget -q https://releases.hashicorp.com/terraform/$VERSION/terraform_${VERSION}_linux_amd64.zip
unzip terraform_${VERSION}_linux_amd64.zip
sudo mv terraform /usr/local/bin/
```
for Windows
```powershell
[Net.ServicePointManager]::SecurityProtocol = 'Tls12, Tls11, Tls, Ssl3'
$VERSION='1.1.5'

invoke-webrequest -uri https://releases.hashicorp.com/terraform/${VERSION}/terraform_${VERSION}_windows_amd64.zip -OutFile terraform_${VERSION}_windows_amd64.zip
```
if the error
```text
invoke-webrequest : The request was aborted: Could not create SSL/TLS securechannel.
```

still shows but the [registry fix](https://devblogs.microsoft.com/nuget/deprecating-tls-1-0-and-1-1-on-nuget-org/#ensuring-your-system-uses-tls-1-2) is not possible,
download `https://releases.hashicorp.com/terraform/1.1.5/terraform_1.1.5_windows_amd64.zip` using the browser.
unzip and copy to some directory (e.g. `c:\tools` ) the `terraform.exe` then add the directory to the `PATH`
```powershell
$env:path="${env:path};C:\tools"
```
to help it prepare `c:\Users\Serguei\.terraform\virtualbox\gold\virtualbox` need to install tar.exe. 
https://docs.microsoft.com/en-us/virtualization/community/team-blog/2017/20171219-tar-and-curl-come-to-windows
http://libarchive.org/downloads/libarchive-v3.5.2-win64.zip
NOTE: the gnu [tar](https://sourceforge.net/projects/gnuwin32/files/tar/1.13-1/tar-1.13-1-bin.zip/download?use_mirror=iweb&download=) is failing in run time.
NOTE: one cannot copy the tar.exe from  Windows 10 to Windows 8 -  the runtime error is:
```text
Program 'tar.exe' failed to run: The specified executable is not a valid application for this OS platform.
```
The only option on Windows 8 is add git binary directory to the `PATH` 

```powershell
$env:PATH="${env:PATH};C:\Program Files\Git\usr\bin"
```
but it is also failing, now from apparently constructing an invalid file path  expression to access the box that is downloaded in the local directory:
```text
2022-02-11T18:39:04.252-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=": C\:\\Users\\Serguei\\.terraform\virtualbox\\gold\virtualbox: Cannot open: No such file or directory tar:"
2022-02-11T18:39:04.257-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr="Error is not recoverable: exiting now

```

The box itself is downloaded fine:
```sh
tar tvf virtualbox.box
-rw-rw-r-- ladar/ladar    2479 2022-02-01 06:13 Vagrantfile
-rw-rw-r-- ladar/ladar    6073 2022-02-01 06:13 box.ovf
-rw-rw-r-- ladar/ladar 103506432 2022-02-01 06:13 generic-alpine39-virtualbox-disk001.vmdk
-rw-rw-r-- ladar/ladar       301 2022-02-01 06:13 info.json
-rw-rw-r-- ladar/ladar        26 2022-02-01 06:13 metadata.json
```

on Windows one can try to switch to CMD to finish initialization:
```cmd
PATH=%PATH%;c:\tools;"C:\Program Files\Git\usr\bin"
terraform.exe init
```
* verify version
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
terraform plan -no-color -out a.out
```
```sh
export TF_LOG=info
terraform apply a.out
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
on Linux 
and

```text
2022-08-09T21:10:25.947-0400 [INFO]  Starting apply for virtualbox_vm.node[0]
virtualbox_vm.node[1]: Creating...
2022-08-09T21:10:25.958-0400 [INFO]  Starting apply for virtualbox_vm.node[1]
virtualbox_vm.node[0]: Still creating... [10s elapsed]
virtualbox_vm.node[1]: Still creating... [10s elapsed]
virtualbox_vm.node[0]: Still creating... [20s elapsed]
virtualbox_vm.node[1]: Still creating... [20s elapsed]
2022-08-09T21:10:49.148-0400 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=tar
2022-08-09T21:10:49.150-0400 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=": C\:\\Users\\Serguei\\.terraform\virtualbox\\gold\virtualbox: Cannot open: No such file or directory"
2022-08-09T21:10:49.157-0400 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr="tar: Error is not recoverable: exiting now"
2022-08-09T21:10:49.193-0400 [ERROR] vertex "virtualbox_vm.node[1]" error: [ERROR] Unpacking image https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box: unpacking gold image virtualbox.box: exit status 2
2022-08-09T21:10:54.677-0400 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=tar
2022-08-09T21:10:54.678-0400 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=": C\:\\Users\\Serguei\\.terraform\virtualbox\\gold\virtualbox: Cannot open: No such file or directory
tar: Error is not recoverable: exiting now"
2022-08-09T21:10:54.706-0400 [ERROR] vertex "virtualbox_vm.node[0]" error: [ERROR] Unpacking image https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box: unpacking gold image virtualbox.box: exit status 2

Error: [ERROR] Unpacking image https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box: unpacking gold image virtualbox.box: exit status 2

  with virtualbox_vm.node[0],
  on virtualbox.tf line 9, in resource "virtualbox_vm" "node":
   9: resource "virtualbox_vm" "node" {


Error: [ERROR] Unpacking image https://app.vagrantup.com/generic/boxes/alpine39/versions/3.6.8/providers/virtualbox.box: unpacking gold image virtualbox.box: exit status 2

  with virtualbox_vm.node[1],
  on virtualbox.tf line 9, in resource "virtualbox_vm" "node":
   9: resource "virtualbox_vm" "node" {


```

on Windows.
On Windows one can untar the box manually into 
if a log was set fo info, there will be some more low level information:

```text
2022-02-11T17:01:36.843-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stderr=60%...70%...80%...100%
2022-02-11T17:01:36.893-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stdout="Clone medium created in format 'VMDK'. UUID: 87afba8f-66a1-4593-bdd8-f522664e0387"
2022-02-11T17:01:57.275-0500 [WARN]  unexpected data: registry.terraform.io/terra-farm/virtualbox:stdout="Waiting for VM "node-02" to power on...VM "node-02" has been successfully started."
```

after a failed run terraform may fail to recognize virtual box images were  still created:


```sh
vboxmanage list vms |grep node\-0
```

```text
"node-01" {db01bdea-5681-4d13-9ecd-a27346f0c85f}
"node-02" {b9b0cd97-508d-4559-92aa-c98124adcb4e}
```
yet
```sh
terraform plan -destroy
```
states
```text
No changes. No objects need to be destroyed.
```

and
```sh
terraform plan -out a.out 
terraform apply a.out 
```
fails with
```text
Error: [ERROR] Create virtualbox VM node-01: machine already exists
Error: [ERROR] Create virtualbox VM node-02: machine already exists
```
### Cleanup
```sh
unset TF_LOG
terraform plan -destroy -out a.out
terraform apply a.out
```
NOTE: all temporary resources will be found in the working directory and need to be added to the `.gitignore`.

If the machine directory `c:\Users\Serguei\.terraform\virtualbox\machine\node-02` was not removed, remove it manually

### See Also

* [install Terraform on Ubuntu Bionic 18.04 Server](https://www.decodingdevops.com/how-to-install-terraform-on-ubuntu-18-04-server/)
* https://turbot.com/v5/docs/7-minute-labs/terraform
