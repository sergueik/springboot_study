```
lsblk -o NAME,SIZE,MODEL,FSTYPE,UUID,MOUNTPOINTS
```
```text
lsblk: unknown column: MOUNTPOINTS
```
```
lsblk -o NAME,SIZE,MODEL,FSTYPE,UUID
```
```

NAME     SIZE MODEL            FSTYPE UUID
sda      477G SAMSUNG MZ7TE512
├─sda1  1000M                  ntfs   A8BC3F8DBC3F54D2
├─sda2   260M                  vfat   4841-54E2
├─sda3  1000M                  vfat   5E42-9783
├─sda4   128M
├─sda5 436.8G                  ntfs   E80445210444F3DA
├─sda6    25G                  ntfs   B0444ED6444E9ECA
└─sda7  12.9G                  ntfs   78BA463BBA45F662
sdb     58.6G Cruzer Fit
├─sdb1     2G                  vfat   4494-CFA6
├─sdb2     1G                  swap   ff0f0548-b1de-4da7-870a-61c7ee7d13db
└─sdb3  53.5G                  ext4   df6307c6-7f8e-4b3c-9d29-3058848f4da3
```

```sh
ls /media/sergueik/LRS_ESP/
```
```text
Boot/                      OneKey/
bootmgr                    System Volume Information/
bootmgr.efi                Version.txt
EFI/
```
```sh
ls /media/sergueik/Windows8_OS/
```
```text
Boot                      Minikube               Ruby24-x64
bootmgr                   Minikube.SAVED         swapfile.sys
BOOTNXT                   opscode               'System Volume Information'
developer                 PerfLogs               temp
'Documents and Settings'   ProgramData            tools
HashiCorp                'Program Files'         Users
hiberfil.sys             'Program Files (x86)'  'Virtual Machines'
Intel                     Recovery               Windows
java                     '$Recycle.Bin'          windows-version.txt
```
```sh
sudo mount /dev/sda7  /media/sergueik/UNKNOWN2
sudo mount /dev/sda6  /media/sergueik/UNKNOWN1
```
```sh
ls /media/sergueik/UNKNOWN1 /media/sergueik/UNKNOWN2
```
```text
/media/sergueik/UNKNOWN1:
Applications   pagefile.sys   'System Volume Information'
Drivers       '$RECYCLE.BIN'
```
```text
/media/sergueik/UNKNOWN2:
OKRBackup   onekey  'System Volume Information'
```

```sh
df  -h /media/sergueik/UNKNOWN?
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda6        25G  4.7G   21G  19% /media/sergueik/UNKNOWN1
/dev/sda7        13G  9.1G  3.8G  71% /media/sergueik/UNKNOWN2
```

* Schrinked lenovo disk to approx 5 G  creating ~ 20 G  unallocated space

```
lsblk -o NAME,SIZE,MODEL,FSTYPE,UUID
```
```text
NAME     SIZE MODEL            FSTYPE UUID
sda      477G SAMSUNG MZ7TE512
├─sda1  1000M                  ntfs   A8BC3F8DBC3F54D2
├─sda2   260M                  vfat   4841-54E2
├─sda3  1000M                  vfat   5E42-9783
├─sda4   128M
├─sda5 436.8G                  ntfs   E80445210444F3DA
├─sda6     5G                  ntfs   B0444ED6444E9ECA
└─sda7  12.9G                  ntfs   78BA463BBA45F662
sdb     58.6G Cruzer Fit
├─sdb1     2G                  vfat   4494-CFA6
├─sdb2     1G                  swap   ff0f0548-b1de-4da7-870a-61c7ee7d13db
└─sdb3  53.5G                  ext4   df6307c6-7f8e-4b3c-9d29-3058848f4da3
```

```sh
sudo parted /dev/sda unit MiB print free
```
```text
Model: ATA SAMSUNG MZ7TE512 (scsi)
Disk /dev/sda: 488386MiB
Sector size (logical/physical): 512B/512B
Partition Table: gpt
Disk Flags:

Number  Start      End        Size       File system  Name                          Flags
        0.02MiB    1.00MiB    0.98MiB    Free Space
 1      1.00MiB    1001MiB    1000MiB    ntfs         Basic data partition          hidden, diag
 2      1001MiB    1261MiB    260MiB     fat32        EFI system partition          boot, hidden, esp
 3      1261MiB    2261MiB    1000MiB    fat32        Basic data partition          hidden
 4      2261MiB    2389MiB    128MiB                  Microsoft reserved partition  msftres
 5      2389MiB    449636MiB  447247MiB  ntfs         Basic data partition          msftdata
        449636MiB  449637MiB  1.00MiB    Free Space
 6      449637MiB  454757MiB  5120MiB    ntfs         Basic data partition          msftdata
        454757MiB  475237MiB  20480MiB   Free Space
 7      475237MiB  488386MiB  13149MiB   ntfs         Basic data partition          hidden, diag
        488386MiB  488386MiB  0.32MiB    Free Space
```

Create new partition on unallocated space
label DOCKER_PARTITION, keep the defaults (ext4)

```sh
lsblk -o NAME,SIZE,MODEL,FSTYPE,UUID
```
```text
sda      477G SAMSUNG MZ7TE512
├─sda1  1000M                  ntfs   A8BC3F8DBC3F54D2
├─sda2   260M                  vfat   4841-54E2
├─sda3  1000M                  vfat   5E42-9783
├─sda4   128M
├─sda5 436.8G                  ntfs   E80445210444F3DA
├─sda6     5G                  ntfs   B0444ED6444E9ECA
├─sda7  12.9G                  ntfs   78BA463BBA45F662
└─sda8    20G                  ext4   0b180fc0-24e7-45dd-9630-00021add73bf
sdb     58.6G Cruzer Fit
├─sdb1     2G                  vfat   4494-CFA6
├─sdb2     1G                  swap   ff0f0548-b1de-4da7-870a-61c7ee7d13db
└─sdb3  53.5G                  ext4   df6307c6-7f8e-4b3c-9d29-3058848f4da3
```
```sh
sudo mkdir -p /data/docker
```
insert in `/etc/fstab`:
```text
UUID=0b180fc0-24e7-45dd-9630-00021add73bf /data/docker ext4 defaults,noatime,nofail 0 2
```
```sh
sudo mount -a 
```
```sh
df -h /data/docker
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda8        20G   24K   19G   1% /data/docker
```

check docker version

```text
Client:
 Version:           20.10.21
 API version:       1.41
 Go version:        go1.18.1
 Git commit:        20.10.21-0ubuntu1~18.04.3
 Built:             Thu Apr 27 05:50:21 2023
 OS/Arch:           linux/amd64
 Context:           default
 Experimental:      true

Server:
 Engine:
  Version:          20.10.21
  API version:      1.41 (minimum version 1.12)
  Go version:       go1.18.1
  Git commit:       20.10.21-0ubuntu1~18.04.3
  Built:            Thu Apr 27 05:36:22 2023
  OS/Arch:          linux/amd64
  Experimental:     false
 containerd:
  Version:          1.6.12-0ubuntu1~18.04.1
  GitCommit:
 runc:
  Version:          1.1.4-0ubuntu1~18.04.2
  GitCommit:
 docker-init:
  Version:          0.19.0
  GitCommit:
```
```
ls -a /etc/docker/
```
```
.  ..  key.json
```
```
ls -a ~sergueik/.docker/
```
```text
.  ..  .buildNodeID  buildx
```

```sh
docker ps
```
```text
              COMMAND                  CREATED       STATUS       PORTS                                                NAMES
ff285934790c   kindest/node:v1.29.2   "/usr/local/bin/entr…"   2 weeks ago   Up 2 hours   127.0.0.1:36977->6443/tcp, 0.0.0.0:8443->30443/tcp   argocd-cluster-control-plane
```
```sh
docker stop ff28
```
```text
ff28
```
```sh
 docker container ls -a
```
```text
CONTAINER ID   IMAGE                  COMMAND                  CREATED       STATUS                        PORTS     NAMES
ff285934790c   kindest/node:v1.29.2   "/usr/local/bin/entr…"   2 weeks ago   Exited (137) 34 seconds ago             argocd-cluster-control-plane
```
```sh
docker image ls
```
```text
REPOSITORY            TAG       IMAGE ID       CREATED       SIZE
kindest/node          <none>    09c50567d34e   2 years ago   956MB
testcontainers/ryuk   0.3.3     64f4b02dc986   5 years ago   12MB
```

this means

__Docker__ installation is relatively straightforward, and the version is old enough and 
don't have the newer containerd image-store complication.


- Docker Engine 20.10.21.
- containerd 1.6.12.
- Ubuntu 18.04 packaging.
- A conventional Docker installation with no existing daemon.json.
- One kind cluster container, now stopped.
- Approximately 968 MB of Docker images.

### NOTE

On Windows, elevated Command Prompt:
diskpart
list volume
select volume N
shrink querymax
exit


Then inspect Event Viewer:
Windows Logs → Application
Source: Microsoft-Windows-Defrag
Event ID: 259


This may identify the exact unmovable file and its cluster location.
```
docker info | grep -E 'Storage Driver|Docker Root Dir'
```
```text
WARNING: No swap limit support
 Storage Driver: overlay2
 Docker Root Dir: /var/lib/docker
```

it is easer to move the blank Docker configuration - no images, no volumes:

```sh
docker ps -a
```
```text
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS    PORTS     NAMES
```
```sh
docker volume ls
```
```text
DRIVER    VOLUME NAME

```
```sh
sudo du -sh /var/lib/docker /var/lib/containerd
```
```text
3.1M    /var/lib/docker
608K    /var/lib/containerd
```
```
sudo mount -a
findmnt /data/docker
df -h /data/docker
```

```text
TARGET       SOURCE    FSTYPE OPTIONS
/data/docker /dev/sda8 ext4   rw,noatime,data=ordered
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda8        20G   24K   19G   1% /data/docker
```
```sh
sudo mkdir -p /etc/docker
sudo vi /etc/docker/daemon.json
```
```json
{
  "data-root": "/data/docker"
}
```
```
sudo mkdir -p /etc/systemd/system/docker.service.d
```
```
sudo vi /etc/systemd/system/docker.service.d/storage.conf
```
```text
[Unit]
RequiresMountsFor=/data/docker
```

```sh
sudo systemctl daemon-reload
```
```
sudo systemctl stop docker
```

```
sudo sudo mv /var/lib/docker /var/lib/docker.OLD
```

```
sudo systemctl start docker
```
```sh
docker info | grep -E 'Storage Driver|Docker Root Dir'
```
```text
WARNING: No swap limit support
 Storage Driver: overlay2
 Docker Root Dir: /data/docker
```
> NOTE: containerd can have separate runtime state under /var/lib/containerd
```sh
sudo ls /var/lib/containerd/
```
```text
io.containerd.content.v1.content   io.containerd.snapshotter.v1.btrfs
io.containerd.metadata.v1.bolt     io.containerd.snapshotter.v1.native
io.containerd.runtime.v1.linux     io.containerd.snapshotter.v1.overlayfs
io.containerd.runtime.v2.task      tmpmounts
io.containerd.snapshotter.v1.aufs
```

```
sudo du -sh !$
```
```text
sudo du -sh /var/lib/containerd/
608K    /var/lib/containerd/
```
```sh
docker pull alpine
```
```sh
sudo find /data/docker/
```
(output truncated):
```
/data/docker/
/data/docker/runtimes
/data/docker/image
/data/docker/image/overlay2
/data/docker/image/overlay2/imagedb
/data/docker/image/overlay2/imagedb/metadata
/data/docker/image/overlay2/imagedb/metadata/sha256
/data/docker/image/overlay2/imagedb/content
/data/docker/image/overlay2/imagedb/content/sha256
/data/docker/image/overlay2/imagedb/content/sha256/3ea06e4cb95e726ea6c6cc4377570ac4c4050d78710004cbd631ca809daf7e8e
/data/docker/image/overlay2/distribution
/data/docker/image/overlay2/distribution/v2metadata-by-diffid
/data/docker/image/overlay2/distribution/v2metadata-by-diffid/sha256
/data/docker/image/overlay2/distribution/v2metadata-by-diffid/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064
/data/docker/image/overlay2/distribution/diffid-by-digest
/data/docker/image/overlay2/distribution/diffid-by-digest/sha256
/data/docker/image/overlay2/distribution/diffid-by-digest/sha256/881fc932939efef04ca120ad44bbd2c0917851eb247aa4995428afb1e80ede17
/data/docker/image/overlay2/layerdb
/data/docker/image/overlay2/layerdb/tmp
/data/docker/image/overlay2/layerdb/sha256
/data/docker/image/overlay2/layerdb/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064
/data/docker/image/overlay2/layerdb/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064/cache-id
/data/docker/image/overlay2/layerdb/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064/tar-split.json.gz
/data/docker/image/overlay2/layerdb/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064/size
/data/docker/image/overlay2/layerdb/sha256/75f1d9657d880da5f75298e8f40e9928bb6e260c50d26f97af196845307e3064/diff
/data/docker/image/overlay2/repositories.json
/data/docker/overlay2
/data/docker/overlay2/l
/data/docker/overlay2/l/REIVC7AMZZ7OAKAS3NVAIFTQ4C
/data/docker/overlay2/9aa1e0e95b851603dfc86b04fd1ca75de4d32b52867352495442b09f8724fbf7
...
/data/docker/overlay2/9aa1e0e95b851603dfc86b04fd1ca75de4d32b52867352495442b09f8724fbf7/diff/mnt
/data/docker/swarm
/data/docker/volumes
/data/docker/volumes/backingFsBlockDev
/data/docker/volumes/metadata.db
/data/docker/buildkit
/data/docker/buildkit/snapshots.db
/data/docker/buildkit/containerdmeta.db
/data/docker/buildkit/cache.db
/data/docker/buildkit/content
/data/docker/buildkit/content/ingest
/data/docker/buildkit/metadata_v2.db
/data/docker/buildkit/executor
/data/docker/containers
/data/docker/tmp
/data/docker/lost+found
/data/docker/trust
/data/docker/plugins
/data/docker/plugins/storage
/data/docker/plugins/storage/ingest
/data/docker/plugins/tmp
/data/docker/network
/data/docker/network/files
/data/docker/network/files/local-kv.db
```
run the container
```sh
docker run -d --name storage-test alpine sleep 300
```
the hash is returned
``` 
2dbe4ddd76962ccc2670145da4f6ae49c8c5d3a5bd5ac66b8c29c48c82c9dcf0
```
```sh
docker inspect --format='{{.GraphDriver.Data.UpperDir}}' storage-test
```
```text
/data/docker/overlay2/961cfc9cb04900c6ab9a0f45402b81a3a7091e921119757976dbceee9034d928/diff
```

confirm configuration survives reboot

```sh
sudo  shutdown -r now
```
monitor the computer to boot successfully
```
docker ps -a
```
```text
CONTAINER ID   IMAGE     COMMAND       CREATED         STATUS                       PORTS     NAMES
2dbe4ddd7696   alpine    "sleep 300"   7 minutes ago   Exited (137) 4 minutes ago             storage-test
```

```sh
docker info | grep -E 'Storage Driver|Docker Root Dir'
```
```text
WARNING: No swap limit support
 Storage Driver: overlay2
 Docker Root Dir: /data/docker
```
```sh
findmnt /data/docker
```
```text
TARGET       SOURCE    FSTYPE OPTIONS
/data/docker /dev/sda8 ext4   rw,noatime,data=ordered

```
```sh
df -h /data/docker
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda8        20G  9.2M   19G   1% /data/docker
```
### TODO 

Hadoop on a Gamer Disappointer Device
  
The Gamer's Disappointment Redemption Project
Turning Lenovo Y40/Y70 gaming disappointments into portable big-data laboratories.

Repository: pocket-hadoop
