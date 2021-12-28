### Info
 practicing basics of running nfs server docker container
https://sysadmins.co.za/setup-a-nfs-server-with-docker/

### Usage

#### Standalone

* pull  the [container image](https://hub.docker.com/r/itsthenetwork/nfs-server-alpine/)
```sh
docker pull itsthenetwork/nfs-server-alpine:latest
```
* on the host create a directory
```sh
mkdir /scratch
chmod 775 /scratch
```
* launch nfs-server
```sh
docker run -d -p 2049:2049 --name nfs-server --privileged -v /scratch:/nfsshare -e SHARED_DIRECTORY=/nfsshare itsthenetwork/nfs-server-alpine:latest
```

* install nfs-client on th host
```sh
sudo apt install nfs-client -y
```

* mount the nfs volume on the host ( use the ip address of the host machine in below):
```sh
sudo mkdir /mnt/nfs
sudo chmod 755  /mnt/nfs

sudo mount -v -o vers=4,loud 192.168.0.92:/ /mnt/nfs
```
write data to `/mnt/nfs` and observe it appear in `/scratch` - curently fails due to lack of permissions for non-root usera -

```sh
sudo touch /mnt/nfs/1.txt
```
check nts server container logs

```text
docker logs nfs-server
```
```sh
ls /scratch
```

```sh
mount | grep nfs4
```
```text
192.168.0.92:/ on /mnt/nfs type nfs4 (rw,relatime,vers=4.0,rsize=1048576,wsize=1048576,namlen=255,hard,proto=tcp,timeo=600,retrans=2,sec=sys,clientaddr=192.168.0.92,local_lock=none,addr=192.168.0.92)

```
#### Build From Scratch (unfinished)


```sh
 docker build -t nfs-server-image  -f Dockerfile  .
docker container rm nfs-server
 docker run --name nfs-server --privileged  -v $(pwd)/exports:/etc/exports:ro nfs-server-image
```
NFS server on __Alpine 3.9__ `Dockerfle` from [ehough/docker-nfs-server](https://github.com/ehough/docker-nfs-server)


https://linux.die.net/man/5/exports
