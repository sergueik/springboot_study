### Install

* follow the [steps](https://phoenixnap.com/kb/install-minikube-on-ubuntu) listed for bionic

```sh
wget https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
```

```sh
sudo cp minikube-linux-amd64 /usr/local/bin/minikube
sudo chmod 755 /usr/local/bin/minikube
```
```sh
curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl
```

```sh
sudo mv ./kubectl /usr/local/bin/kubectl
sudo chmod 755 /usr/local/bin/kubectl
```
* start the VM
```sh
minikube start --driver=virtualbox
```
```cmd
minikube.exe start --driver=virtualbox
```
if seeing the communication timeout
```cmd
X minikube is unable to connect to the VM: dial tcp 192.168.59.100:22: i/o timeout
```
rerun the command one or more times
```cmd
minikube stop
minikube.exe start --driver=virtualbox
```
from CMD whell

if still seeing error and /or if there is no  kubectl or kubeadm inside the vm, recycle it and let minikube create it:
```sh
kubectl version
```
```text
 kubectl: command not found
```
```sh
which kubecl
```
no output
```cmd
minikube delete --all --purge
```

```text
* Deleting "minikube" in virtualbox ...
* Removed all traces of the "minikube" cluster.
```
```cmd
minikube.exe start --driver=virtualbox

```
until the success completion message:

```
```text
* minikube v1.30.1 on Microsoft Windows 8.1 6.3.9600.20778 Build 9600.20778
  - MINIKUBE_IP=192.168.59.100
* Using the virtualbox driver based on user configuration
* Starting control plane node minikube in cluster minikube
* Downloading Kubernetes v1.26.3 preload ...
    > preloaded-images-k8s-v18-v1...:  397.02 MiB / 397.02 MiB  100.00% 27.45 M
* Creating virtualbox VM (CPUs=2, Memory=4000MB, Disk=20000MB) ...
* Found network options:
  - NO_PROXY=192.168.99.100,192.168.99.101
  - no_proxy=192.168.99.100,192.168.99.101
* Preparing Kubernetes v1.26.3 on Docker 20.10.23 ...
  - env NO_PROXY=192.168.99.100,192.168.99.101
  - Generating certificates and keys ...
  - Booting up control plane ...
  - Configuring RBAC rules ...
* Configuring bridge CNI (Container Networking Interface) ...
  - Using image gcr.io/k8s-minikube/storage-provisioner:v5
* Enabled addons: default-storageclass, storage-provisioner
* Verifying Kubernetes components...

! C:\Minikube\kubectl.exe is version 1.22.2, which may have incompatibilities with Kubernetes 1.26.3.
  - Want kubectl v1.26.3? Try 'minikube kubectl -- get pods -A'
* Done! kubectl is now configured to use "minikube" cluster and "default" namespace by default
```
or (on Linux host)
```sh
minikube start --driver=docker
```
NOTE: Vagrant driver is a lot quicker on Windows / SSD than on Linux/CF
NOTE: to use `--driver=none` one appears to have to install minikube as root

*  smoke test
```sh
kubectl create deployment nginx --image=nginx:alpine
```
get ip address
```sh
kubectl get pods -o name
```
```text
pod/nginx-565785f75c-9v9jm
```
to get bare name, use `jsonpath` argument
```sh
kubectl get pods -o jsonpath="{.items[*].metadata.name}"
```
```text
nginx-6c557cc74d-zcfhl
```
```sh
kubectl get deployments -o name
```
```text
deployment.apps/nginx
```
```sh
kubectl get deployments -o jsonpath="{.items[*].metadata.name}"
```
```text
nginx
```
```sh
kubectl get pods -o jsonpath="{.items[*].spec.containers[*].image}"
```
```text
nginx:alpine
```
in the following command replace the pod id with the otutput
```sh
kubectl get pod/nginx-6c557cc74d-zcfhl -o jsonpath="{.status.podIP}"
```
```text
10.244.0.3
```
```text
172.17.0.4
```
alternatively
```sh
kubectl get pod nginx-565785f75c-9kp98 --template="{{.status.podIP}}"
```
```text
172.17.0.2
```
- but `template` argument does not support array indexing.

NOTE: kubectl will not like the command 
```sh
kubectl get pod pod/nginx-565785f75c-9v9jm
```
```text
error: there is no need to specify a resource type as a separate argument when passing arguments in resource/name form (e.g. 'kubectl get resource/<resource_name>' instead of 'kubectl get resource resource/<resource_name>'
```
the bare name is OK:
```sh
kubectl get pod nginx-565785f75c-9kp98
```
```text
NAME                     READY   STATUS    RESTARTS   AGE
nginx-565785f75c-9kp98   1/1     Running   0          6h7m
```
NOTE:
there will be timeout when accessing nginx directly in the pod:
```sh
curl http://172.17.0.4:80/
```

```cmd
kubectl get deployment
```
```text
NAME    READY   UP-TO-DATE   AVAILABLE   AGE
nginx   1/1     1            1           11m
```

```cmd
kubectl expose deployment nginx --type=NodePort --port=80
```
```text
service/nginx exposed
```
```cmd
kubectl get svc
NAME         TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
kubernetes   ClusterIP   10.96.0.1       <none>        443/TCP        21m
nginx        NodePort    10.102.11.125   <none>        80:32683/TCP   103s
```
connect to the mapped port on the host with ip adress of `minikube ip`
```cmd
curl http://192.168.59.100:32683
```
```text
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   615  100   615    0     0   159k      0 --:--:-- --:--:-- --:--:--  200k<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
<style>
html { color-scheme: light dark; }
body { width: 35em; margin: 0 auto;
font-family: Tahoma, Verdana, Arial, sans-serif; }
</style>
</head>
<body>
<h1>Welcome to nginx!</h1>
<p>If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.</p>

<p>For online documentation and support please refer to
<a href="http://nginx.org/">nginx.org</a>.<br/>
Commercial support is available at
<a href="http://nginx.com/">nginx.com</a>.</p>

<p><em>Thank you for using nginx.</em></p>
</body>
</html>
```
#### Export VirtualBox MAchine XML
```cmd
set path=%PATH%;"c:\Program Files\Oracle\VirtualBox"
vboxmanage showvminfo "minikube"  > minikue.config.txt
type minikue.config.txt
```
```text
Name:            minikube
Groups:          /
Guest OS:        Linux 2.6 / 3.x / 4.x (64-bit)
UUID:            a8bb48c1-af2a-47bb-b11c-ec0a9ef0465f
Config file:     C:\Users\Serguei\.minikube\machines\minikube\minikube\minikube.
vbox
Snapshot folder: C:\Users\Serguei\.minikube\machines\minikube\minikube\Snapshots

Log folder:      C:\Users\Serguei\.minikube\machines\minikube\minikube\Logs
Hardware UUID:   a8bb48c1-af2a-47bb-b11c-ec0a9ef0465f
Memory size:     4000MB
Page Fusion:     off
VRAM size:       8MB
CPU exec cap:    100%
HPET:            on
Chipset:         piix3
Firmware:        BIOS
Number of CPUs:  2
PAE:             on
Long Mode:       on
Triple Fault Reset: off
APIC:            on
X2APIC:          off
CPUID Portability Level: 0
CPUID overrides: None
Boot menu mode:  disabled
Boot Device (1): DVD
Boot Device (2): DVD
Boot Device (3): HardDisk
Boot Device (4): Not Assigned
ACPI:            on
IOAPIC:          on
BIOS APIC mode:  APIC
Time offset:     0ms
RTC:             UTC
Hardw. virt.ext: on
Nested Paging:   on
Large Pages:     on
VT-x VPID:       on
VT-x unr. exec.: on
Paravirt. Provider: Default
Effective Paravirt. Provider: KVM
State:           running (since 2025-06-20T08:10:13.653000000)
Monitor count:   1
3D Acceleration: off
2D Video Acceleration: off
Teleporter Enabled: off
Teleporter Port: 0
Teleporter Address:
Teleporter Password:
Tracing Enabled: off
Allow Tracing to Access VM: off
Tracing Configuration:
Autostart Enabled: off
Autostart Delay: 0
Default Frontend:
Storage Controller Name (0):            SATA
Storage Controller Type (0):            IntelAhci
Storage Controller Instance Number (0): 0
Storage Controller Max Port Count (0):  30
Storage Controller Port Count (0):      30
Storage Controller Bootable (0):        on
SATA (0, 0): C:\Users\Serguei\.minikube\machines\minikube\boot2docker.iso (UUID:
 969b1bc1-683f-467f-be21-312360fa8fcf)
SATA (1, 0): C:\Users\Serguei\.minikube\machines\minikube\disk.vmdk (UUID: ccd4c
fd3-d3b0-431a-86c9-7d9dc330ced6)
NIC 1:           MAC: 080027705228, Attachment: NAT, Cable connected: on, Trace:
 off (file: none), Type: virtio, Reported speed: 0 Mbps, Boot priority: 0, Promi
sc Policy: deny, Bandwidth group: none
NIC 1 Settings:  MTU: 0, Socket (send: 64, receive: 64), TCP Window (send:64, re
ceive: 64)
NIC 1 Rule(0):   name = ssh, protocol = tcp, host ip = 127.0.0.1, host port = 49
500, guest ip = , guest port = 22
NIC 2:           MAC: 08002740970D, Attachment: Host-only Interface 'VirtualBox
Host-Only Ethernet Adapter #3', Cable connected: on, Trace: off (file: none), Ty
pe: virtio, Reported speed: 0 Mbps, Boot priority: 0, Promisc Policy: deny, Band
width group: none
NIC 3:           disabled
NIC 4:           disabled
NIC 5:           disabled
NIC 6:           disabled
NIC 7:           disabled
NIC 8:           disabled
Pointing Device: PS/2 Mouse
Keyboard Device: PS/2 Keyboard
UART 1:          disabled
UART 2:          disabled
UART 3:          disabled
UART 4:          disabled
LPT 1:           disabled
LPT 2:           disabled
Audio:           enabled (Driver: DSOUND, Controller: AC97, Codec: STAC9700)
Audio playback:  disabled
Audio capture: disabled
Clipboard Mode:  disabled
Drag and drop Mode: disabled
Session name:    headless
Video mode:      720x400x0 at 0,0 enabled
VRDE:            disabled
USB:             disabled
EHCI:            disabled
XHCI:            disabled

USB Device Filters:

<none>

Available remote USB devices:

<none>

Currently Attached USB Devices:

<none>

Bandwidth groups:  <none>

Shared folders:

Name: 'c/Users', Host path: '\\?\c:\Users' (machine mapping), writable

VRDE Connection:    not active
Clients so far:     0

Capturing:          not active
Capture audio:      not active
Capture screens:    0
Capture file:       C:\Users\Serguei\.minikube\machines\minikube\minikube\miniku
be.webm
Capture dimensions: 1024x768
Capture rate:       512 kbps
Capture FPS:        25
Capture options:

Guest:

Configured memory balloon size:      0 MB
OS type:                             Linux26_64
Additions run level:                 2
Additions version:                   6.0.0 r127566


Guest Facilities:

Facility "VirtualBox Base Driver": active/running (last update: 2025/06/20 08:10
:36 UTC)
Facility "VirtualBox System Service": active/running (last update: 2025/06/20 08
:10:36 UTC)
Facility "Seamless Mode": not active (last update: 2025/06/20 08:10:36 UTC)
Facility "Graphics Mode": not active (last update: 2025/06/20 08:10:36 UTC)

```

##### Linux Install

* Mount Windows Disk on fuse (manual or auto)
```
```
* Add minikube VM (only need to do once):  
```sh
vboxmanage registervm /media/sergueik/Windows8_OS/Users/Serguei/.minikube/machines/minikube/minikube/minikube.vbox
```
* Create Host-Only network
```sh
vboxmanage hostonlyif create 
vboxmanage hostonlyif ipconfig "Virtual Box Host-Only Network #3" --ip 192.168.58.1 --netmask 255.255.255.0
```

the later command returns an error
```text
VBoxManage: error: The host network interface named 'Virtual Box Host-Only Network #3' could not be found
```
so replace argument:
```sh
vboxmanage hostonlyif ipconfig vboxnet0 --ip 192.168.59.1 --netmask 255.255.255.0
```
```sh
sudo kill $(ps ax | grep -i vboxsvc | awk '{print $1}')
```
```sh
vboxmanage modifyvm "minikube" --nic2 hostonly --hostonlyadapter2 "Virtual Box Host-Only Network #3"
```
```sh
vboxmanage startvm "minikube" --type=headless
```
```sh
curl -LO https://storage.googleapis.com/releases/latest/minikube-linux-amd64

sudo install minikube-linux-amd64  /usr/local/bin/minikube

```
```text
<?xml version='1.0' encoding='UTF-8'?><Error><Code>UserProjectAccountProblem</Code><Message>The project to be billed is associated with a closed billing account.</Message><Details>The billing account for the owning project is disabled in state closed</Details></Error>  
```
```sh
curl -LO https://github.com/kubernetes/minikube/releases/latest/download/minikube_latest_amd64.deb

sudo dpkg -i minikube_latest_amd64.deb
```
```sh
minikube start -driver=virtualbox --image-repository=registry.k8.io
```

```text
minikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
E0620 12:15:24.745999    4908 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
E0620 12:15:24.746488    4908 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
✨  Using the virtualbox driver based on existing profile
👍  Starting "minikube" primary control-plane node in "minikube" cluster
❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
🔥  Deleting "minikube" in virtualbox ...
🤦  StartHost failed, but will try again: creating host: create: creating: Error setting up host only network on machine start: The host-only adapter we just created is not visible. This is a well known VirtualBox bug. You might want to uninstall it and reinstall at least version 5.0.12 that is is supposed to fix this issue
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
😿  Failed to start virtualbox VM. Running "minikube delete" may fix it: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one

❌  Exiting due to IF_VBOX_SAME_IP: Failed to start host: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
💡  Suggestion: Use VirtualBox to remove the conflicting VM and/or network interfaces
📘  Documentation: https://stackoverflow.com/questions/55573426/virtualbox-is-configured-with-multiple-host-only-adapters-with-the-same-ip-whe
🍿  Related issue: https://github.com/kubernetes/minikube/issues/3584



```

modified `vboxnet0` to use `192.168.59.`1`

the error becomes
```text
inikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
✨  Using the virtualbox driver based on user configuration
✅  Using image repository registry.k8.io
💿  Downloading VM boot image ...
    > minikube-v1.36.0-amd64.iso....:  65 B / 65 B [---------] 100.00% ? p/s 0s
    > minikube-v1.36.0-amd64.iso:  254.76 KiB / 360.83 MiB [>___] 0.07% ? p/s     > minikube-v1.36.0-amd64.iso:  1.69 MiB / 360.83 MiB [>_____] 0.47% ? p/s     > minikube-v1.36.0-amd64.iso:  6.16 MiB / 360.83 MiB [>_____] 1.71% ? p/s     > minikube-v1.36.0-amd64.iso:  11.84 MiB / 360.83 MiB  3.28% 19.32 MiB p/s    > minikube-v1.36.0-amd64.iso:  16.28 MiB / 360.83 MiB  4.51% 19.32 MiB p/s    > minikube-v1.36.0-amd64.iso:  22.53 MiB / 360.83 MiB  6.24% 19.32 MiB p/s    > minikube-v1.36.0-amd64.iso:  28.94 MiB / 360.83 MiB  8.02% 19.91 MiB p/s    > minikube-v1.36.0-amd64.iso:  35.41 MiB / 360.83 MiB  9.81% 19.91 MiB p/s    > minikube-v1.36.0-amd64.iso:  40.87 MiB / 360.83 MiB  11.33% 19.91 MiB p/    > minikube-v1.36.0-amd64.iso:  47.25 MiB / 360.83 MiB  13.09% 20.59 MiB p/    > minikube-v1.36.0-amd64.iso:  50.81 MiB / 360.83 MiB  14.08% 20.59 MiB p/    > minikube-v1.36.0-amd64.iso:  57.14 MiB / 360.83 MiB  15.84% 20.59 MiB p/    > minikube-v1.36.0-amd64.iso:  63.78 MiB / 360.83 MiB  17.68% 21.04 MiB p/    > minikube-v1.36.0-amd64.iso:  70.41 MiB / 360.83 MiB  19.51% 21.04 MiB p/    > minikube-v1.36.0-amd64.iso:  76.66 MiB / 360.83 MiB  21.24% 21.04 MiB p/    > minikube-v1.36.0-amd64.iso:  81.25 MiB / 360.83 MiB  22.52% 21.57 MiB p/    > minikube-v1.36.0-amd64.iso:  87.81 MiB / 360.83 MiB  24.34% 21.57 MiB p/    > minikube-v1.36.0-amd64.iso:  94.56 MiB / 360.83 MiB  26.21% 21.57 MiB p/    > minikube-v1.36.0-amd64.iso:  100.78 MiB / 360.83 MiB  27.93% 22.27 MiB p    > minikube-v1.36.0-amd64.iso:  107.12 MiB / 360.83 MiB  29.69% 22.27 MiB p    > minikube-v1.36.0-amd64.iso:  112.31 MiB / 360.83 MiB  31.13% 22.27 MiB p    > minikube-v1.36.0-amd64.iso:  118.97 MiB / 360.83 MiB  32.97% 22.79 MiB p    > minikube-v1.36.0-amd64.iso:  125.34 MiB / 360.83 MiB  34.74% 22.79 MiB p    > minikube-v1.36.0-amd64.iso:  131.84 MiB / 360.83 MiB  36.54% 22.79 MiB p    > minikube-v1.36.0-amd64.iso:  135.59 MiB / 360.83 MiB  37.58% 23.11 MiB p    > minikube-v1.36.0-amd64.iso:  138.78 MiB / 360.83 MiB  38.46% 23.11 MiB p    > minikube-v1.36.0-amd64.iso:  145.03 MiB / 360.83 MiB  40.19% 23.11 MiB p    > minikube-v1.36.0-amd64.iso:  151.66 MiB / 360.83 MiB  42.03% 23.35 MiB p    > minikube-v1.36.0-amd64.iso:  158.50 MiB / 360.83 MiB  43.93% 23.35 MiB p    > minikube-v1.36.0-amd64.iso:  165.37 MiB / 360.83 MiB  45.83% 23.35 MiB p    > minikube-v1.36.0-amd64.iso:  172.75 MiB / 360.83 MiB  47.87% 24.11 MiB p    > minikube-v1.36.0-amd64.iso:  177.78 MiB / 360.83 MiB  49.27% 24.11 MiB p    > minikube-v1.36.0-amd64.iso:  184.41 MiB / 360.83 MiB  51.11% 24.11 MiB p    > minikube-v1.36.0-amd64.iso:  190.87 MiB / 360.83 MiB  52.90% 24.50 MiB p    > minikube-v1.36.0-amd64.iso:  197.44 MiB / 360.83 MiB  54.72% 24.50 MiB p    > minikube-v1.36.0-amd64.iso:  202.84 MiB / 360.83 MiB  56.21% 24.50 MiB p    > minikube-v1.36.0-amd64.iso:  209.37 MiB / 360.83 MiB  58.02% 24.91 MiB p    > minikube-v1.36.0-amd64.iso:  216.06 MiB / 360.83 MiB  59.88% 24.91 MiB p    > minikube-v1.36.0-amd64.iso:  223.44 MiB / 360.83 MiB  61.92% 24.91 MiB p    > minikube-v1.36.0-amd64.iso:  230.25 MiB / 360.83 MiB  63.81% 25.54 MiB p    > minikube-v1.36.0-amd64.iso:  237.06 MiB / 360.83 MiB  65.70% 25.54 MiB p    > minikube-v1.36.0-amd64.iso:  244.12 MiB / 360.83 MiB  67.66% 25.54 MiB p    > minikube-v1.36.0-amd64.iso:  250.66 MiB / 360.83 MiB  69.47% 26.09 MiB p    > minikube-v1.36.0-amd64.iso:  257.66 MiB / 360.83 MiB  71.41% 26.09 MiB p    > minikube-v1.36.0-amd64.iso:  264.53 MiB / 360.83 MiB  73.31% 26.09 MiB p    > minikube-v1.36.0-amd64.iso:  271.19 MiB / 360.83 MiB  75.16% 26.62 MiB p    > minikube-v1.36.0-amd64.iso:  278.00 MiB / 360.83 MiB  77.04% 26.62 MiB p    > minikube-v1.36.0-amd64.iso:  283.66 MiB / 360.83 MiB  78.61% 26.62 MiB p    > minikube-v1.36.0-amd64.iso:  290.37 MiB / 360.83 MiB  80.47% 26.97 MiB p    > minikube-v1.36.0-amd64.iso:  297.09 MiB / 360.83 MiB  82.33% 26.97 MiB p    > minikube-v1.36.0-amd64.iso:  303.12 MiB / 360.83 MiB  84.01% 26.97 MiB p    > minikube-v1.36.0-amd64.iso:  305.12 MiB / 360.83 MiB  84.56% 26.81 MiB p    > minikube-v1.36.0-amd64.iso:  311.28 MiB / 360.83 MiB  86.27% 26.81 MiB p    > minikube-v1.36.0-amd64.iso:  317.91 MiB / 360.83 MiB  88.10% 26.81 MiB p    > minikube-v1.36.0-amd64.iso:  324.97 MiB / 360.83 MiB  90.06% 27.21 MiB p    > minikube-v1.36.0-amd64.iso:  331.47 MiB / 360.83 MiB  91.86% 27.21 MiB p    > minikube-v1.36.0-amd64.iso:  337.00 MiB / 360.83 MiB  93.39% 27.21 MiB p    > minikube-v1.36.0-amd64.iso:  344.22 MiB / 360.83 MiB  95.39% 27.53 MiB p    > minikube-v1.36.0-amd64.iso:  350.41 MiB / 360.83 MiB  97.11% 27.53 MiB p    > minikube-v1.36.0-amd64.iso:  357.53 MiB / 360.83 MiB  99.08% 27.53 MiB p    > minikube-v1.36.0-amd64.iso:  360.83 MiB / 360.83 MiB  100.00% 30.33 MiB p
👍  Starting "minikube" primary control-plane node in "minikube" cluster
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.

❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.
🔥  Deleting "minikube" in virtualbox ...


🤦  StartHost failed, but will try again: creating host: create: creating: Error setting up host only network on machine start: The host-only adapter we just created is not visible. This is a well known VirtualBox bug. You might want to uninstall it and reinstall at least version 5.0.12 that is is supposed to fix this issue
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
😿  Failed to start virtualbox VM. Running "minikube delete" may fix it: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one

❌  Exiting due to IF_VBOX_SAME_IP: Failed to start host: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
💡  Suggestion: Use VirtualBox to remove the conflicting VM and/or network interfaces
📘  Documentation: https://stackoverflow.com/questions/55573426/virtualbox-is-configured-with-multiple-host-only-adapters-with-the-same-ip-whe
🍿  Related issue: https://github.com/kubernetes/minikube/issues/3584

sergueik@lenovoy40-1:~$ 
sergueik@lenovoy40-1:~$ 
sergueik@lenovoy40-1:~$ minikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
E0620 12:12:52.485698    4298 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
E0620 12:12:52.485892    4298 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
✨  Using the virtualbox driver based on existing profile
👍  Starting "minikube" primary control-plane node in "minikube" cluster
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) .../ ❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.

❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
🔥  Deleting "minikube" in virtualbox ...
🤦  StartHost failed, but will try again: creating host: create: creating: Error setting up host only network on machine start: The host-only adapter we just created is not visible. This is a well known VirtualBox bug. You might want to uninstall it and reinstall at least version 5.0.12 that is is supposed to fix this issue
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
😿  Failed to start virtualbox VM. Running "minikube delete" may fix it: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one

❌  Exiting due to IF_VBOX_SAME_IP: Failed to start host: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
💡  Suggestion: Use VirtualBox to remove the conflicting VM and/or network interfaces
📘  Documentation: https://stackoverflow.com/questions/55573426/virtualbox-is-configured-with-multiple-host-only-adapters-with-the-same-ip-whe
🍿  Related issue: https://github.com/kubernetes/minikube/issues/3584

sergueik@lenovoy40-1:~$ minikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
E0620 12:13:45.879686    4685 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
E0620 12:13:45.880089    4685 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
✨  Using the virtualbox driver based on existing profile
👍  Starting "minikube" primary control-plane node in "minikube" cluster
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.

❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
🤦  StartHost failed, but will try again: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
😿  Failed to start virtualbox VM. Running "minikube delete" may fix it: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one

❌  Exiting due to IF_VBOX_SAME_IP: Failed to start host: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
💡  Suggestion: Use VirtualBox to remove the conflicting VM and/or network interfaces
📘  Documentation: https://stackoverflow.com/questions/55573426/virtualbox-is-configured-with-multiple-host-only-adapters-with-the-same-ip-whe
🍿  Related issue: https://github.com/kubernetes/minikube/issues/3584

sergueik@lenovoy40-1:~$ minikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
E0620 12:15:24.745999    4908 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
E0620 12:15:24.746488    4908 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
✨  Using the virtualbox driver based on existing profile
👍  Starting "minikube" primary control-plane node in "minikube" cluster
❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
🔥  Deleting "minikube" in virtualbox ...
🤦  StartHost failed, but will try again: creating host: create: creating: Error setting up host only network on machine start: The host-only adapter we just created is not visible. This is a well known VirtualBox bug. You might want to uninstall it and reinstall at least version 5.0.12 that is is supposed to fix this issue
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...
😿  Failed to start virtualbox VM. Running "minikube delete" may fix it: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one

❌  Exiting due to IF_VBOX_SAME_IP: Failed to start host: creating host: create: precreate: VirtualBox is configured with multiple host-only adapters with the same IP "192.168.58.1". Please remove one
💡  Suggestion: Use VirtualBox to remove the conflicting VM and/or network interfaces
📘  Documentation: https://stackoverflow.com/questions/55573426/virtualbox-is-configured-with-multiple-host-only-adapters-with-the-same-ip-whe
🍿  Related issue: https://github.com/kubernetes/minikube/issues/3584

sergueik@lenovoy40-1:~$ vboxmanage hostonlyif ipconfig vboxnet0 --ip 192.168.59.1 --netmask 255.255.255.0
sergueik@lenovoy40-1:~$ 
sergueik@lenovoy40-1:~$ minikube start --driver=virtualbox --image-repository=registry.k8.io
😄  minikube v1.36.0 on Ubuntu 18.04
E0620 12:21:31.510329    5593 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
E0620 12:21:31.510604    5593 start.go:819] api.Load failed for minikube: filestore "minikube": Docker machine "minikube" does not exist. Use "docker-machine ls" to list machines. Use "docker-machine create" to add a new one.
✨  Using the virtualbox driver based on existing profile
👍  Starting "minikube" primary control-plane node in "minikube" cluster
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...❗  The image 'registry.k8.io/coredns/coredns:v1.12.0' was not found; unable to add it to cache.

❗  The image 'registry.k8.io/pause:3.10' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/k8s-minikube/storage-provisioner:v5' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-scheduler:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/etcd:3.5.21-0' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-apiserver:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-proxy:v1.33.1' was not found; unable to add it to cache.
❗  The image 'registry.k8.io/kube-controller-manager:v1.33.1' was not found; unable to add it to cache.
❗  Image was not built for the current minikube version. To resolve this you can delete and recreate your minikube cluster using the latest images. Expected minikube version: v1.35.0 -> Actual minikube version: v1.36.0
❗  Failing to connect to https://registry.k8.io/ from both inside the minikube VM and host machine
💡  To pull new external images, you may need to configure a proxy: https://minikube.sigs.k8s.io/docs/reference/networking/proxy/
🐳  Preparing Kubernetes v1.33.1 on Docker 28.0.4 ...
❌  Unable to load cached images: LoadCachedImages: stat /home/sergueik/.minikube/cache/images/amd64/registry.k8.io/etcd_3.5.21-0: no such file or directory
⌛  Another minikube instance is downloading dependencies... | ⌛  Another minikube instance is downloading dependencies... ⌛  Another minikube instance is    > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubeadm.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubectl.sha256:  64 B / 64 B [--------------------] 100.00% 42 B p/s 1.7s
    > kubeadm.sha256:  64 B / 64 B [--------------------] 100.00% 41 B p/s 1.8s
    > kubelet.sha256:  64 B / 64 B [------------------------->] 100.00% ? p/s     > kubelet.sha256:  64 B / 64 B [--------------------] 100.00% 39 B p/s 1.9s
    > kubeadm:  512.00 KiB / 71.08 MiB [>_______________________] 0.70% ? p/s     > kubectl:  288.00 KiB / 57.34 MiB [>_______________________] 0.49% ? p/s     > kubelet:  303.67 KiB / 77.91 MiB [>_______________________] 0.38% ? p/s     > kubeadm:  3.28 MiB / 71.08 MiB [->________________________] 4.61% ? p/s     > kubectl:  795.00 KiB / 57.34 MiB [>_______________________] 1.35% ? p/s     > kubelet:  712.56 KiB / 77.91 MiB [>_______________________] 0.89% ? p/s     > kubeadm:  6.62 MiB / 71.08 MiB [-->_______________________] 9.32% ? p/s     > kubectl:  1.31 MiB / 57.34 MiB [>_________________________] 2.29% ? p/s     > kubelet:  1.19 MiB / 77.91 MiB [>_________________________] 1.53% ? p/s     > kubeadm:  10.48 MiB / 71.08 MiB [->_________] 14.75% 16.64 MiB p/s ETA 3    > kubectl:  1.92 MiB / 57.34 MiB [>____________] 3.35% 2.73 MiB p/s ETA 20    > kubelet:  1.71 MiB / 77.91 MiB [>____________] 2.19% 2.35 MiB p/s ETA 32    > kubeadm:  14.95 MiB / 71.08 MiB [-->________] 21.03% 16.64 MiB p/s ETA 3    > kubectl:  2.51 MiB / 57.34 MiB [>____________] 4.38% 2.73 MiB p/s ETA 20    > kubelet:  2.21 MiB / 77.91 MiB [>____________] 2.83% 2.35 MiB p/s ETA 32    > kubeadm:  19.32 MiB / 71.08 MiB [-->________] 27.19% 16.64 MiB p/s ETA 3    > kubectl:  3.28 MiB / 57.34 MiB [>____________] 5.72% 2.73 MiB p/s ETA 19    > kubelet:  2.85 MiB / 77.91 MiB [>____________] 3.65% 2.35 MiB p/s ETA 31    > kubeadm:  24.34 MiB / 71.08 MiB [--->_______] 34.25% 17.06 MiB p/s ETA 2    > kubectl:  3.96 MiB / 57.34 MiB [>____________] 6.91% 2.77 MiB p/s ETA 19    > kubelet:  3.48 MiB / 77.91 MiB [>____________] 4.47% 2.39 MiB p/s ETA 31    > kubeadm:  28.72 MiB / 71.08 MiB [---->______] 40.40% 17.06 MiB p/s ETA 2    > kubectl:  4.73 MiB / 57.34 MiB [->___________] 8.24% 2.77 MiB p/s ETA 18    > kubelet:  4.12 MiB / 77.91 MiB [>____________] 5.29% 2.39 MiB p/s ETA 30    > kubeadm:  33.72 MiB / 71.08 MiB [----->_____] 47.43% 17.06 MiB p/s ETA 2    > kubectl:  5.48 MiB / 57.34 MiB [->___________] 9.55% 2.77 MiB p/s ETA 18    > kubelet:  4.79 MiB / 77.91 MiB [>____________] 6.15% 2.39 MiB p/s ETA 30    > kubeadm:  38.46 MiB / 71.08 MiB [----->_____] 54.11% 17.48 MiB p/s ETA 1    > kubectl:  6.20 MiB / 57.34 MiB [->__________] 10.82% 2.84 MiB p/s ETA 18    > kubelet:  5.40 MiB / 77.91 MiB [>____________] 6.94% 2.44 MiB p/s ETA 29    > kubeadm:  43.62 MiB / 71.08 MiB [------>____] 61.36% 17.48 MiB p/s ETA 1    > kubectl:  7.08 MiB / 57.34 MiB [->__________] 12.34% 2.84 MiB p/s ETA 17    > kubelet:  6.09 MiB / 77.91 MiB [->___________] 7.82% 2.44 MiB p/s ETA 29    > kubeadm:  49.34 MiB / 71.08 MiB [------->___] 69.42% 17.48 MiB p/s ETA 1    > kubectl:  7.87 MiB / 57.34 MiB [->__________] 13.73% 2.84 MiB p/s ETA 17    > kubelet:  6.78 MiB / 77.91 MiB [->___________] 8.70% 2.44 MiB p/s ETA 29    > kubeadm:  53.56 MiB / 71.08 MiB [-------->__] 75.35% 17.97 MiB p/s ETA 0    > kubectl:  8.64 MiB / 57.34 MiB [->__________] 15.07% 2.91 MiB p/s ETA 16    > kubelet:  7.42 MiB / 77.91 MiB [->___________] 9.52% 2.50 MiB p/s ETA 28    > kubeadm:  58.24 MiB / 71.08 MiB [--------->_] 81.93% 17.97 MiB p/s ETA 0    > kubectl:  9.48 MiB / 57.34 MiB [->__________] 16.54% 2.91 MiB p/s ETA 16    > kubelet:  8.15 MiB / 77.91 MiB [->__________] 10.47% 2.50 MiB p/s ETA 27    > kubeadm:  63.27 MiB / 71.08 MiB [--------->_] 89.00% 17.97 MiB p/s ETA 0    > kubectl:  10.44 MiB / 57.34 MiB [-->________] 18.20% 2.91 MiB p/s ETA 16    > kubelet:  8.96 MiB / 77.91 MiB [->__________] 11.50% 2.50 MiB p/s ETA 27    > kubeadm:  68.40 MiB / 71.08 MiB [---------->] 96.23% 18.41 MiB p/s ETA 0    > kubectl:  11.44 MiB / 57.34 MiB [-->________] 19.95% 3.03 MiB p/s ETA 15    > kubelet:  9.77 MiB / 77.91 MiB [->__________] 12.55% 2.59 MiB p/s ETA 26    > kubeadm:  71.08 MiB / 71.08 MiB [------------] 100.00% 22.61 MiB p/s 3.3s
    > kubectl:  13.22 MiB / 57.34 MiB [-->________] 23.05% 3.03 MiB p/s ETA 14    > kubelet:  11.44 MiB / 77.91 MiB [->_________] 14.68% 2.59 MiB p/s ETA 25    > kubectl:  15.94 MiB / 57.34 MiB [--->_______] 27.80% 3.03 MiB p/s ETA 13    > kubelet:  13.89 MiB / 77.91 MiB [->_________] 17.82% 2.59 MiB p/s ETA 24    > kubectl:  18.64 MiB / 57.34 MiB [--->_______] 32.51% 3.61 MiB p/s ETA 10    > kubelet:  16.18 MiB / 77.91 MiB [-->________] 20.77% 3.12 MiB p/s ETA 19    > kubectl:  21.86 MiB / 57.34 MiB [---->_______] 38.12% 3.61 MiB p/s ETA 9    > kubelet:  19.00 MiB / 77.91 MiB [-->________] 24.39% 3.12 MiB p/s ETA 18    > kubectl:  25.52 MiB / 57.34 MiB [----->______] 44.50% 3.61 MiB p/s ETA 8    > kubelet:  22.00 MiB / 77.91 MiB [--->_______] 28.24% 3.12 MiB p/s ETA 17    > kubectl:  29.05 MiB / 57.34 MiB [------>_____] 50.66% 4.49 MiB p/s ETA 6    > kubelet:  25.12 MiB / 77.91 MiB [--->_______] 32.25% 3.88 MiB p/s ETA 13    > kubectl:  32.87 MiB / 57.34 MiB [------>_____] 57.33% 4.49 MiB p/s ETA 5    > kubelet:  28.31 MiB / 77.91 MiB [--->_______] 36.34% 3.88 MiB p/s ETA 12    > kubectl:  37.06 MiB / 57.34 MiB [------->____] 64.64% 4.49 MiB p/s ETA 4    > kubelet:  31.63 MiB / 77.91 MiB [---->______] 40.60% 3.88 MiB p/s ETA 11    > kubectl:  40.73 MiB / 57.34 MiB [-------->___] 71.04% 5.46 MiB p/s ETA 3    > kubelet:  34.89 MiB / 77.91 MiB [----->______] 44.78% 4.67 MiB p/s ETA 9    > kubectl:  44.48 MiB / 57.34 MiB [--------->__] 77.57% 5.46 MiB p/s ETA 2    > kubelet:  38.50 MiB / 77.91 MiB [----->______] 49.42% 4.67 MiB p/s ETA 8    > kubectl:  47.87 MiB / 57.34 MiB [---------->_] 83.49% 5.46 MiB p/s ETA 1    > kubelet:  42.08 MiB / 77.91 MiB [------>_____] 54.01% 4.67 MiB p/s ETA 7    > kubectl:  50.66 MiB / 57.34 MiB [---------->_] 88.35% 6.17 MiB p/s ETA 1    > kubelet:  45.03 MiB / 77.91 MiB [------>_____] 57.80% 5.46 MiB p/s ETA 6    > kubectl:  52.92 MiB / 57.34 MiB [----------->] 92.29% 6.17 MiB p/s ETA 0    > kubelet:  47.00 MiB / 77.91 MiB [------->____] 60.32% 5.46 MiB p/s ETA 5    > kubectl:  56.32 MiB / 57.34 MiB [----------->] 98.23% 6.17 MiB p/s ETA 0    > kubelet:  50.40 MiB / 77.91 MiB [------->____] 64.70% 5.46 MiB p/s ETA 5    > kubectl:  57.34 MiB / 57.34 MiB [-------------] 100.00% 9.78 MiB p/s 6.1s
    > kubelet:  54.44 MiB / 77.91 MiB [-------->___] 69.87% 6.12 MiB p/s ETA 3    > kubelet:  57.29 MiB / 77.91 MiB [-------->___] 73.54% 6.12 MiB p/s ETA 3    > kubelet:  61.14 MiB / 77.91 MiB [--------->__] 78.48% 6.12 MiB p/s ETA 2    > kubelet:  65.43 MiB / 77.91 MiB [---------->_] 83.98% 6.91 MiB p/s ETA 1    > kubelet:  69.23 MiB / 77.91 MiB [---------->_] 88.87% 6.91 MiB p/s ETA 1    > kubelet:  73.68 MiB / 77.91 MiB [----------->] 94.58% 6.91 MiB p/s ETA 0    > kubelet:  77.87 MiB / 77.91 MiB [----------->] 99.95% 7.80 MiB p/s ETA 0    > kubelet:  77.91 MiB / 77.91 MiB [------------] 100.00% 10.82 MiB p/s 7.4s

💢  initialization failed, will try again: wait: /bin/bash -c "sudo env PATH="/var/lib/minikube/binaries/v1.33.1:$PATH" kubeadm init --config /var/tmp/minikube/kubeadm.yaml  --ignore-preflight-errors=DirAvailable--etc-kubernetes-manifests,DirAvailable--var-lib-minikube,DirAvailable--var-lib-minikube-etcd,FileAvailable--etc-kubernetes-manifests-kube-scheduler.yaml,FileAvailable--etc-kubernetes-manifests-kube-apiserver.yaml,FileAvailable--etc-kubernetes-manifests-kube-controller-manager.yaml,FileAvailable--etc-kubernetes-manifests-etcd.yaml,Port-10250,Swap,NumCPU,Mem": Process exited with status 1
stdout:
[init] Using Kubernetes version: v1.33.1
[preflight] Running pre-flight checks
[preflight] Pulling images required for setting up a Kubernetes cluster
[preflight] This might take a minute or two, depending on the speed of your internet connection
[preflight] You can also perform this action beforehand using 'kubeadm config images pull'

stderr:
	[WARNING Service-Kubelet]: kubelet service is not enabled, please run 'systemctl enable kubelet.service'
error execution phase preflight: [preflight] Some fatal errors occurred:
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: server misbehaving
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-proxy:v1.33.1: failed to pull image registry.k8.io/kube-proxy:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: server misbehaving
	[ERROR ImagePull]: failed to pull image registry.k8.io/coredns:v1.12.0: failed to pull image registry.k8.io/coredns:v1.12.0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/pause:3.10: failed to pull image registry.k8.io/pause:3.10: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/etcd:3.5.21-0: failed to pull image registry.k8.io/etcd:3.5.21-0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
[preflight] If you know what you are doing, you can make a check non-fatal with `--ignore-preflight-errors=...`
To see the stack trace of this error execute with --v=5 or higher


💣  Error starting cluster: wait: /bin/bash -c "sudo env PATH="/var/lib/minikube/binaries/v1.33.1:$PATH" kubeadm init --config /var/tmp/minikube/kubeadm.yaml  --ignore-preflight-errors=DirAvailable--etc-kubernetes-manifests,DirAvailable--var-lib-minikube,DirAvailable--var-lib-minikube-etcd,FileAvailable--etc-kubernetes-manifests-kube-scheduler.yaml,FileAvailable--etc-kubernetes-manifests-kube-apiserver.yaml,FileAvailable--etc-kubernetes-manifests-kube-controller-manager.yaml,FileAvailable--etc-kubernetes-manifests-etcd.yaml,Port-10250,Swap,NumCPU,Mem": Process exited with status 1
stdout:
[init] Using Kubernetes version: v1.33.1
[preflight] Running pre-flight checks
[preflight] Pulling images required for setting up a Kubernetes cluster
[preflight] This might take a minute or two, depending on the speed of your internet connection
[preflight] You can also perform this action beforehand using 'kubeadm config images pull'

stderr:
	[WARNING Service-Kubelet]: kubelet service is not enabled, please run 'systemctl enable kubelet.service'
error execution phase preflight: [preflight] Some fatal errors occurred:
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-proxy:v1.33.1: failed to pull image registry.k8.io/kube-proxy:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/coredns:v1.12.0: failed to pull image registry.k8.io/coredns:v1.12.0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/pause:3.10: failed to pull image registry.k8.io/pause:3.10: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/etcd:3.5.21-0: failed to pull image registry.k8.io/etcd:3.5.21-0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
[preflight] If you know what you are doing, you can make a check non-fatal with `--ignore-preflight-errors=...`
To see the stack trace of this error execute with --v=5 or higher


╭───────────────────────────────────────────────────────────────────────────────────────────╮
│                                                                                           │
│    😿  If the above advice does not help, please let us know:                             │
│    👉  https://github.com/kubernetes/minikube/issues/new/choose                           │
│                                                                                           │
│    Please run `minikube logs --file=logs.txt` and attach logs.txt to the GitHub issue.    │
│                                                                                           │
╰───────────────────────────────────────────────────────────────────────────────────────────╯

❌  Exiting due to INET_LOOKUP_HOST: wait: /bin/bash -c "sudo env PATH="/var/lib/minikube/binaries/v1.33.1:$PATH" kubeadm init --config /var/tmp/minikube/kubeadm.yaml  --ignore-preflight-errors=DirAvailable--etc-kubernetes-manifests,DirAvailable--var-lib-minikube,DirAvailable--var-lib-minikube-etcd,FileAvailable--etc-kubernetes-manifests-kube-scheduler.yaml,FileAvailable--etc-kubernetes-manifests-kube-apiserver.yaml,FileAvailable--etc-kubernetes-manifests-kube-controller-manager.yaml,FileAvailable--etc-kubernetes-manifests-etcd.yaml,Port-10250,Swap,NumCPU,Mem": Process exited with status 1
stdout:
[init] Using Kubernetes version: v1.33.1
[preflight] Running pre-flight checks
[preflight] Pulling images required for setting up a Kubernetes cluster
[preflight] This might take a minute or two, depending on the speed of your internet connection
[preflight] You can also perform this action beforehand using 'kubeadm config images pull'

stderr:
	[WARNING Service-Kubelet]: kubelet service is not enabled, please run 'systemctl enable kubelet.service'
error execution phase preflight: [preflight] Some fatal errors occurred:
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: failed to pull image registry.k8.io/kube-apiserver:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: failed to pull image registry.k8.io/kube-controller-manager:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: failed to pull image registry.k8.io/kube-scheduler:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/kube-proxy:v1.33.1: failed to pull image registry.k8.io/kube-proxy:v1.33.1: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/coredns:v1.12.0: failed to pull image registry.k8.io/coredns:v1.12.0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/pause:3.10: failed to pull image registry.k8.io/pause:3.10: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
	[ERROR ImagePull]: failed to pull image registry.k8.io/etcd:3.5.21-0: failed to pull image registry.k8.io/etcd:3.5.21-0: Error response from daemon: Get "https://registry.k8.io/v2/": dial tcp: lookup registry.k8.io on 10.0.2.3:53: no such host
[preflight] If you know what you are doing, you can make a check non-fatal with `--ignore-preflight-errors=...`
To see the stack trace of this error execute with --v=5 or higher

💡  Suggestion: Verify that your HTTP_PROXY and HTTPS_PROXY environment variables are set correctly.
📘  Documentation: https://minikube.sigs.k8s.io/docs/handbook/vpn_and_proxy/


```
```sh
minikube status
```
```text
E0620 12:26:39.168043    6385 status.go:458] kubeconfig endpoint: get endpoint: "minikube" does not appear in /home/sergueik/.kube/config
minikube
type: Control Plane
host: Running
kubelet: Stopped
apiserver: Stopped
kubeconfig: Misconfigured


WARNING: Your kubectl is pointing to stale minikube-vm.
To fix the kubectl context, run `minikube update-context`

```

```sh
minikube delete

```
```text

🔥  Deleting "minikube" in virtualbox ...
💀  Removed all traces of the "minikube" cluster.
```
```sh
minikube start --driver=virtualbox 
```
```txt
😄  minikube v1.36.0 on Ubuntu 18.04
✨  Using the virtualbox driver based on user configuration
👍  Starting "minikube" primary control-plane node in "minikube" cluster
💾  Downloading Kubernetes v1.33.1 preload ...
    > preloaded-images-k8s-v18-v1...:  366.76 KiB / 347.04 MiB [] 0.10% ? p/s     > preloaded-images-k8s-v18-v1...:  2.00 MiB / 347.04 MiB [>_] 0.58% ? p/s     > preloaded-images-k8s-v18-v1...:  6.84 MiB / 347.04 MiB [>_] 1.97% ? p/s     > preloaded-images-k8s-v18-v1...:  13.31 MiB / 347.04 MiB  3.84% 21.61 MiB    > preloaded-images-k8s-v18-v1...:  18.94 MiB / 347.04 MiB  5.46% 21.61 MiB    > preloaded-images-k8s-v18-v1...:  25.78 MiB / 347.04 MiB  7.43% 21.61 MiB    > preloaded-images-k8s-v18-v1...:  32.53 MiB / 347.04 MiB  9.37% 22.28 MiB    > preloaded-images-k8s-v18-v1...:  39.44 MiB / 347.04 MiB  11.36% 22.28 Mi    > preloaded-images-k8s-v18-v1...:  46.41 MiB / 347.04 MiB  13.37% 22.28 Mi    > preloaded-images-k8s-v18-v1...:  53.31 MiB / 347.04 MiB  15.36% 23.08 Mi    > preloaded-images-k8s-v18-v1...:  60.22 MiB / 347.04 MiB  17.35% 23.08 Mi    > preloaded-images-k8s-v18-v1...:  67.34 MiB / 347.04 MiB  19.41% 23.08 Mi    > preloaded-images-k8s-v18-v1...:  72.84 MiB / 347.04 MiB  20.99% 23.69 Mi    > preloaded-images-k8s-v18-v1...:  79.66 MiB / 347.04 MiB  22.95% 23.69 Mi    > preloaded-images-k8s-v18-v1...:  86.47 MiB / 347.04 MiB  24.92% 23.69 Mi    > preloaded-images-k8s-v18-v1...:  93.69 MiB / 347.04 MiB  27.00% 24.40 Mi    > preloaded-images-k8s-v18-v1...:  100.47 MiB / 347.04 MiB  28.95% 24.40 M    > preloaded-images-k8s-v18-v1...:  107.12 MiB / 347.04 MiB  30.87% 24.40 M    > preloaded-images-k8s-v18-v1...:  114.53 MiB / 347.04 MiB  33.00% 25.07 M    > preloaded-images-k8s-v18-v1...:  121.25 MiB / 347.04 MiB  34.94% 25.07 M    > preloaded-images-k8s-v18-v1...:  127.78 MiB / 347.04 MiB  36.82% 25.07 M    > preloaded-images-k8s-v18-v1...:  132.16 MiB / 347.04 MiB  38.08% 25.34 M    > preloaded-images-k8s-v18-v1...:  137.81 MiB / 347.04 MiB  39.71% 25.34 M    > preloaded-images-k8s-v18-v1...:  143.81 MiB / 347.04 MiB  41.44% 25.34 M    > preloaded-images-k8s-v18-v1...:  149.72 MiB / 347.04 MiB  43.14% 25.60 M    > preloaded-images-k8s-v18-v1...:  153.09 MiB / 347.04 MiB  44.11% 25.60 M    > preloaded-images-k8s-v18-v1...:  156.56 MiB / 347.04 MiB  45.11% 25.60 M    > preloaded-images-k8s-v18-v1...:  163.94 MiB / 347.04 MiB  47.24% 25.48 M    > preloaded-images-k8s-v18-v1...:  170.31 MiB / 347.04 MiB  49.08% 25.48 M    > preloaded-images-k8s-v18-v1...:  176.19 MiB / 347.04 MiB  50.77% 25.48 M    > preloaded-images-k8s-v18-v1...:  182.41 MiB / 347.04 MiB  52.56% 25.82 M    > preloaded-images-k8s-v18-v1...:  189.50 MiB / 347.04 MiB  54.60% 25.82 Mi

    > preloaded-images-k8s-v18-v1...:  195.97 MiB / 347.04 MiB  56.47% 25.82 M    > preloaded-images-k8s-v18-v1...:  347.04 MiB / 347.04 MiB  100.00% 19.03     > preloaded-images-k8s-v18-v1...:  347.04 MiB / 347.04 MiB  100.00% 19.03 M
🔥  Creating virtualbox VM (CPUs=2, Memory=3900MB, Disk=20000MB) ...\ 

❗  Image was not built for the current minikube version. To resolve this you can delete and recreate your minikube cluster using the latest images. Expected minikube version: v1.35.0 -> Actual minikube version: v1.36.0
🐳  Preparing Kubernetes v1.33.1 on Docker 28.0.4 ...
    ▪ Generating certificates and keys ...
    ▪ Booting up control plane ...
    ▪ Configuring RBAC rules ...
🔗  Configuring bridge CNI (Container Networking Interface) ...
🔎  Verifying Kubernetes components...
    ▪ Using image gcr.io/k8s-minikube/storage-provisioner:v5
🌟  Enabled addons: default-storageclass, storage-provisioner
💡  kubectl not found. If you need it, try: 'minikube kubectl -- get pods -A'
🏄  Done! kubectl is now configured to use "minikube" cluster and "default" namespace by default

```
```sh
minikube kubectl get ns

```
```text
NAME              STATUS   AGE
default           Active   4m39s
kube-node-lease   Active   4m39s
kube-public       Active   4m39s
kube-system       Active   4m39s
```
```sh
kubectl create deployment nginx --image=nginx:alpine
```
```text
deployment.apps/nginx created
```
```sh

minikube kubectl -- expose deployment nginx --type=NodePort --port=80
```
```text
deployment.apps/nginx created
```
```sh
minikube kubectl -- get svc
```
```text

NAME         TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)        AGE
kubernetes   ClusterIP   10.96.0.1      <none>        443/TCP        11m
nginx        NodePort    10.108.15.59   <none>        80:30853/TCP   102s```
```
```sh
curl -s $(minikube ip):30853
```
```text
<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
<style>
html { color-scheme: light dark; }
body { width: 35em; margin: 0 auto;
font-family: Tahoma, Verdana, Arial, sans-serif; }
</style>
</head>
<body>
<h1>Welcome to nginx!</h1>
<p>If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.</p>

<p>For online documentation and support please refer to
<a href="http://nginx.org/">nginx.org</a>.<br/>
Commercial support is available at
<a href="http://nginx.com/">nginx.com</a>.</p>

<p><em>Thank you for using nginx.</em></p>
</body>
</html>

```
```sh
minikube kubectl -- delete deployment nginx
```
```text

deployment.apps "nginx" deleted
```
```sh
minikube kubectl -- delete svc nginx
```
```text
service "nginx" deleted
```
```sh
minikube kubectl -- get pod
```
```text
No resources found in default namespace.
```
```text
curl: (28) Failed to connect to 172.17.0.4 port 80: Timed out
```

* add service
```sh
kubectl expose deployment nginx --type=NodePort --port=80
```
```sh
kubectl get services
```
```text
NAME         TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)        AGE
kubernetes   ClusterIP   10.96.0.1      <none>        443/TCP        226d
nginx        NodePort    10.102.35.42   <none>        80:30469/TCP   12s
```
```sh
kubectl get service/nginx -o json
```
```json
{
    "apiVersion": "v1",
    "kind": "Service",
    "metadata": {
        "creationTimestamp": "2022-08-09T21:22:51Z",
        "labels": {
            "app": "nginx"
        },
        "name": "nginx",
        "namespace": "default",
        "resourceVersion": "58861",
        "uid": "69201b82-b89d-4dae-a8b7-8269019f5d76"
    },
    "spec": {
        "clusterIP": "10.102.35.42",
        "clusterIPs": [
            "10.102.35.42"
        ],
        "externalTrafficPolicy": "Cluster",
        "internalTrafficPolicy": "Cluster",
        "ipFamilies": [
            "IPv4"
        ],
        "ipFamilyPolicy": "SingleStack",
        "ports": [
            {
                "nodePort": 30469,
                "port": 80,
                "protocol": "TCP",
                "targetPort": 80
            }
        ],
        "selector": {
            "app": "nginx"
        },
        "sessionAffinity": "None",
        "type": "NodePort"
    },
    "status": {
        "loadBalancer": {}
    }
}
```
```sh
kubectl get service/nginx -o jsonpath="{.spec.selector}"
```
```json
{"app":"nginx"}
```
```sh
curl $(minikube service nginx --url)
kubectl delete service nginx
kubectl delete deployment nginx
```
```
<h1>Welcome to nginx!</h1>
```

* test basic deployment
```
kubectl apply -f deployment.nginx.yaml
```
```text
deployment.apps/nginx-app created
```

```sh
kubectl get pod | grep nginx-app
```
```text
NAME                         READY   STATUS    RESTARTS   AGE
nginx-app-64d5fbc4b6-lc84r   1/1     Running   0          5s
```
(the suffix will vary)
```sh
kubectl expose deployment nginx-app --type=NodePort --port=80
```
```sh
curl $(minikube service nginx-app --url)
```
```html
<h1>Welcome to nginx!</h1>
```
```sh
kubectl delete service nginx-app
kubectl delete deployment nginx-app
```

### Downward API volume

* deploy just a pod with a [downward API volume](https://kubernetes.io/docs/tasks/inject-data-application/downward-api-volume-expose-pod-information/#capabilities-of-the-downward-api)

```sh
kubectl create -f downwardapi-example.yaml
```

```sh
kubectl get pod downwardapi-example
```
```
NAME                  READY   STATUS    RESTARTS   AGE
downwardapi-example   1/1     Running   0          37s
```

```sh
kubectl logs downwardapi-example
```
```text
labels:
cluster="info"
annotations:
build="info"
kubernetes.io/config.seen="2022-08-23T19:45:29.457804390Z"
```

these lines will continue be repeatedy logged

### Deploying Locally Built Images

* this is work in progrss. current status is `ErrImagePull`/`ImagePullBackOff`:
```sh

NAME=basic-perl-cgi
docker build -t $NAME ../$NAME
docker image ls | grep $NAME

DEPLOYMENT_NAME=perl-app
kubectl apply -f deployment.$DEPLOYMENT_NAME.yaml
kubectl get pod
```
NOTE: do not provide `-f Dockerfile` argument - confuses Docker.

```txt
NAME                        READY   STATUS             RESTARTS   AGE
perl-app-79f77c8665-45zjs   0/1     ImagePullBackOff   0          3m58s
```
```sh
kubectl delete deployment $DEPLOYMENT_NAME
```

* point your terminal to use the docker daemon inside minikube:

```sh
eval $(minikube docker-env)
```
on Windows pay attention to setting the `PATH` to `kibectl.exe` directory
```cmd
minikube start --driver=virtualbox
PATH=%PATH%;%USERPROFILE%
for /f "tokens=*" %. IN ('minikube.exe -p minikube docker-env --shell cmd') do %.
```
build custom image inside minikube  docker to [make it accessible](https://minikube.sigs.k8s.io/docs/handbook/pushing/) to minikube kubernetes cluster

```sh
NAME=basic-perl-cgi
docker build -t $NAME ../$NAME
docker image ls | grep $NAME
```

```cmd
set NAME=basic-perl-cgi
docker build -t %NAME% ..\%NAME%
docker image ls | find %NAME%
```
NOTE: do not provide `-f Dockerfile` argument - confuses Docker.

* configure deployment `imagePullPolicy` in `deployment.perl-app.yaml` to [uses a private image](https://kubernetes.io/docs/concepts/containers/images/)

```yaml
  spec:
      containers:
      - name: perl-app
        image: basic-perl-cgi-container
        # https://www.tutorialworks.com/kubernetes-imagepullbackoff/
        # image: hashicorp/http-echo:0.2.3
        # https://sysdig.com/blog/debug-kubernetes-crashloopbackoff/
        imagePullPolicy: IfNotPresent

```
* repeat deployment commands
```sh
DEPLOYMENT_NAME=perl-app

kubectl apply -f deployment.$DEPLOYMENT_NAME.yaml
```
```cmd
set DEPLOYMENT_NAME=perl-app
kubectl apply -f deployment.%DEPLOYMENT_NAME%.yaml
```

* see

```text
kubectl get pod | grep $DEPLOYMENT_NAME
perl-app-858d8d56fc-dqldl   1/1     Running   0          8s
```
```sh
kubectl expose deployment $DEPLOYMENT_NAME --type=NodePort --port=8080 --target-port=80
```
```cmd
kubectl expose deployment %DEPLOYMENT_NAME% --type=NodePort --port=8080 --target-port=80
```

```sh
curl $(minikube service $DEPLOYMENT_NAME --url)/cgi-bin/list.cgi
```

```cmd
for /f "tokens=*" %. IN ('minikube.exe service %DEPLOYMENT_NAME% --url') do "c:\Program Files\Git\mingw64\bin\curl.exe" %./cgi-bin/list.cgi
```

```json
{
   "results" : [
      {
         "text" : "ut enim ad minim veniam"
      },
      {
         "text" : "sed do eiusmod tempor incididunt"
      },
      {
         "text" : "Lorem ipsum dolor sit amet"
      },
      {
         "text" : "Lorem ipsum dolor sit amet"
      },
      {
         "text" : "quis nostrud exercitation ullamco"
      }
   ]
}
```
alternatively  check `$DOCKER_HOST` and the port assiged
```sh
kubectl get service $DEPLOYMENT_NAME
```
```text
NAME       TYPE       CLUSTER-IP      EXTERNAL-IP   PORT(S)          AGE
perl-app   NodePort   10.100.74.243   <none>        8080:32497/TCP   9s

```
if anything

```cmd
for /f "tokens=*" %. IN ('kubectl.exe get pod -o name ^| findstr %DEPLOYMENT_NAME%') do @kubectl.exe exec -it %. -- sh
```
```sh
docker exec -it $(kubectl.exe get pod -o name | grep $DEPLOYMENT_NAME) -- sh
```
alternatively
```sh
docker container ls | grep $IMAGE_NAME
```

NOTE: there will be 2 containers:
```text
eb85c44ea5b9   511a7faa7aee           "sh -c 'PIDFILE='/ru…"   14 minutes ago   Up 14 minutes             k8s_perl-app_perl-app-858d8d56fc-k56dx_default_18ad70be-7c53-4d4b-8acb-569f2b8f96d0_0
a48d3418ac34   k8s.gcr.io/pause:3.5   "/pause"                 14 minutes ago   Up 14 minutes             k8s_POD_perl-app-858d8d56fc-k56dx_default_18ad70be-7c53-4d4b-8acb-569f2b8f96d0_0
```
therefore
```sh
docker exec -it $(docker container ls | grep $IMAGE_NAME| grep -v POD | awk '{print $1}') sh
```
in the container check
```sh
curl http://localhost:80/cgi-bin/list.cgi
```
```json
{
   "results" : [
      {
         "text" : "sed do eiusmod tempor incididunt"
      },
      {
         "text" : "Duis aute irure dolor in reprehenderit in"
      },
      {
         "text" : "quis nostrud exercitation ullamco"
      },
      {
         "text" : "voluptate velit esse cillum dolore eu fugiat nulla pariatur"
      },
      {
         "text" : "excepteur sint occaecat cupidatat non proident"
      }
   ]
}
```
### Cleanup
```sh
kubectl delete deployment $DEPLOYMENT_NAME
kubectl delete service $DEPLOYMENT_NAME
```
* confirm
```sh
kubectl get replicaset
```
```text
No resources found in default namespace.
```
```sh
docker container prune -f
docker image rm $NAME
```
```sh
minikube stop
```
### NOTE
speeding up
```sh
ln -s -T  '/media/sergueik/Windows8_OS/Virtual Machines/disk.vmdk' disk.vmdk
```
### NOTE

To use Docker commands in Minukube VM easiest (with __8.1__ and older Windows) is to install the [Docker ToolBox](https://github.com/docker-archive/toolbox)

### Helm

to practice Helm

```sh
minikube addons enable ingress
```

### Errors

```text
X Exiting due to K8S_UNHEALTHY_CONTROL_PLANE: wait 6m0s for node: wait for healthy API server: controlPlane never updated to v1.22.3
```
solved by rerunning the command

### Minikube NFS Volume
* boot and connect to alpine nfs server
```sh
docker pull phico/nfs-server
```
* install nfs-server with volume mounted against `/var/nfs`
* enable ingress
```sh
minikube addons enable ingress
```
* create network
```sh
docker network connect minikube nfs-server
```
* create deployment
```sh
kubectl apply -f deployment-alpine-volume.yaml
```
confirm the log is being appended:
```sh
tail -f /var/nfs/exports/log.txt
```
* install helm
```sh
wget https://get.helm.sh/helm-v3.7.2-linux-amd64.tar.gz
tar xzvf helm-v3.7.2-linux-amd64.tar.gz
sudo cp linux-amd64/helm /usr/local/bin/helm
sudo chmod +x /usr/local/bin/helm
```
for configuring the alpine nts-server __itsthenetwork/nfs-server-alpine__ to work the way __phico/nfs-server__ does, see 'nfs-server.run.sh'
* cleanup
```sh
docker container stop nfs-server
docker container rm nfs-server
minikube stop
```

### NOTE

if using Powershell to execute kubernetes commands reading input from the local files make sure not to omit the encoding override argument.
Otherwise the __UTF16__ BOM (`<feff>`) will be stored

* wrong way

```powershell
'admin'| out-file -nonewline -literalpath username.txt
'1f2d1e2e67df' | out-file -nonewline -literalpath password.txt
```
```powershell
C:\Minikube\kubectl.exe create secret generic db-user-pass  --from-file=./username.txt --from-file=./password.txt
```

```powershell
$s = C:\Minikube\kubectl.exe get secrets/db-user-pass -o jsonpath="{.data.password}"
```

```powershell
$s
```

```text
//4xAGYAMgBkADEAZQAyAGUANgA3AGQAZgA=
```

```powershell
[System.Text.Encoding]::Unicode.GetString([System.Convert]::FromBase64String($s))
```
this will print
```text
□﻿﻿﻿1f2d1e2e67df
```
* save to  another file, again without specifying the encoding
```powershell
[System.Text.Encoding]::Unicode.GetString([System.Convert]::FromBase64String($s)) | out-file -nonewline -literalpath result.txt
```

open in the editor
```text
<feff>1f2d1e2e67df
```
 the BOM will nnot bi visible in the notepad

* repeat with `encoding=ascii` option

```powershell
[System.Text.Encoding]::Unicode.GetString([System.Convert]::FromBase64String($s)) | out-file -nonewline -literalpath result2.txt -encoding ascii
```
this time the BOM character will be converted to a question mark:

```powershell
get-content  -path .\result2.txt
```
```text
?1f2d1e2e67df
```
* verify that passing encoding argument and updating the conversion call fixes it:
+ step 1
```powershell
'admin'| out-file -nonewline -literalpath username.txt -encoding ascii -force
'1f2d1e2e67df' | out-file -nonewline -literalpath password.txt -encoding ascii -force
```

+ step 2
```powrshell
$s = C:\Minikube\kubectl.exe get secrets/db-user-pass -o jsonpath="{.data.password}"
  ```

+ step 3
```powershell
[System.Text.Encoding]::Ascii.GetString([System.Convert]::FromBase64String($s))
```

```text
1f2d1e2e67df
```
### See Also

   * https://github.com/DanWahlin/DockerAndKubernetesCourseCode/tree/main/samples
   * https://v1-18.docs.kubernetes.io/docs/setup/learning-environment/minikube/
   * [kubernetes dicumentation](https://kubernetes.io/docs/concepts/containers/images/)
   * https://www.tutorialworks.com/kubernetes-imagepullbackoff/
   * https://sysdig.com/blog/debug-kubernetes-crashloopbackoff/
   * [Play with Kubernetes](https://labs.play-with-k8s.com)
   * [Play with Kubernetes workshop](https://training.play-with-kubernetes.com/kubernetes-workshop/)
   * [Play with Kubernetes Classroom](https://training.play-with-kubernetes.com)
   * [Kubernetes Tutorials](https://github.com/mrbobbytables/k8s-intro-tutorials)
   * __Packaging Applications with Helm for Kubernetes__ [example source](https://github.com/phcollignon/helm3)
   * __Deploying Statefull Application to Kubernetes__ [example source](https://github.com/phcollignon/kubernetes_storage)
   * [NFS server conected to minukube cluster](https://github.com/phcollignon/nfs-server-minikube)
   * [kubernetes in docker](https://github.com/phcollignon/kind)
   * [repository](https://github.com/scriptcamp/vagrant-kubeadm-kubernetes) `Vagrantfile` to Setup Kubernetes 3 node pod Cluster on Ubuntu 18.05 Vagrant VMs (a little resource requirement heavy)
   * [discussion](https://stackoverflow.com/questions/41166622/how-to-select-a-specific-pod-for-a-service-in-kubernetes/41171197#41171197) of statefulsets
   * [dicussion](https://stackoverflow.com/questions/53212748/kubernetes-route-incoming-traffic-to-specific-pod/63487152#63487152) on routing
   * [kubernetes deployments versus statefulsets](https://www.baeldung.com/ops/kubernetes-deployment-vs-statefulsets)
   * [RedHat StatefulSets Kubernetes Tutorial](https://redhat-scholars.github.io/kubernetes-tutorial/kubernetes-tutorial/statefulset.html)
   * https://kubernetes.io/docs/tasks/access-application-cluster/list-all-running-container-images/
   * [minikube with knative](https://github.com/ivangfr/knative-minikube-environment) intended to host the deploy and run some Serverless applications
   * filtering `kubectl` outputs via `field selecors` [documentation](https://kubernetes.io/docs/concepts/overview/working-with-objects/field-selectors/)
   * [multi-container pods in Kubernetes](https://linchpiner.github.io/k8s-multi-container-pods.html)
   * [extending applications on Kubernetes with multi-container pods](https://learnk8s.io/sidecar-containers-patterns)
   * [multi-container pods in Kubernetes](https://k21academy.com/docker-kubernetes/multi-container-pods/)
   * [sidecar container best practices and examples](https://www.containiq.com/post/kubernetes-sidecar-container)	
   * GKE documentation

       + [https://cloud.google.com/kubernetes-engine/docs/concepts#core-concepts](https://cloud.google.com/kubernetes-engine/docs/concepts#core-concepts)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/pod](https://cloud.google.com/kubernetes-engine/docs/concepts/pod)  
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/deployment](https://cloud.google.com/kubernetes-engine/docs/concepts/deployment) 
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/service](https://cloud.google.com/kubernetes-engine/docs/concepts/service)
       + [https://cloud.google.com/kubernetes-engine/docs/how-to/exposing-apps](https://cloud.google.com/kubernetes-engine/docs/how-to/exposing-apps)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/volumes](https://cloud.google.com/kubernetes-engine/docs/concepts/volumes)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/daemonset](https://cloud.google.com/kubernetes-engine/docs/concepts/daemonset) 
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/statefulset](https://cloud.google.com/kubernetes-engine/docs/concepts/statefulset) 
       + [https://kubernetes.io/docs/concepts/workloads/controllers/replicaset/](https://kubernetes.io/docs/concepts/workloads/controllers/replicaset/)
       + [https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller/](https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller/)   (older)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/secret](https://cloud.google.com/kubernetes-engine/docs/concepts/secret)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/configmap](https://cloud.google.com/kubernetes-engine/docs/concepts/configmap)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-overview](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-overview)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/cluster-autoscaler](https://cloud.google.com/kubernetes-engine/docs/concepts/cluster-autoscaler)
       + [https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-autoscaler](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-autoscaler)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/horizontalpodautoscaler](https://cloud.google.com/kubernetes-engine/docs/concepts/horizontalpodautoscaler)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/verticalpodautoscaler](https://cloud.google.com/kubernetes-engine/docs/concepts/verticalpodautoscaler) (unclear!)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/ingress-ilb](https://cloud.google.com/kubernetes-engine/docs/concepts/ingress-ilb)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/ingress-xlb](https://cloud.google.com/kubernetes-engine/docs/concepts/ingress-xlb)
       + [https://cloud.google.com/kubernetes-engine/docs/how-to/load-balance-ingress](https://cloud.google.com/kubernetes-engine/docs/how-to/load-balance-ingress)
       + [https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balance-ingress](https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balance-ingress)
       + [https://cloud.google.com/kubernetes-engine/docs/concepts/service](https://cloud.google.com/kubernetes-engine/docs/concepts/service)


   * Misc.
     + Jenkins / K8 (mostly in Russian, possibly translations):
        - [part 1](https://habr.com/ru/post/487922)
        - [part 2](https://habr.com/ru/articles/488796)
        - [part 3](https://habr.com/ru/articles/490302)
        - [part 4](https://habr.com/ru/articles/493580)


     + [k8 minikube docs](https://minikube.sigs.k8s.io/docs/start/)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
