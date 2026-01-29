### Info

This direcrory contains basic example of container with static ip address
as explained in [Assign static IP to Docker container](https://stackoverflow.com/questions/27937185/assign-static-ip-to-docker-container) post

### Usage

```sh
docker-compose up -d
```
will respond with

```sh
Creating static-ip-container ... done
```
```sh
for CONTAINER in $(docker-compose ps -q); do echo -e $(docker inspect -f "{{.Name}}" $CONTAINER) \
"\t" \
$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $CONTAINER); \
done
```
```text
/static-ip-container 	 172.20.128.2
```
alternatively
```sh
CONTAINER_ID=$(docker-compose ps -q|head -1)
NAME=$(docker inspect $CONTAINER_ID | jq -r '.[]|.Name')
IPADDRESS=$(docker inspect $CONTAINER_ID | jq -r '.[] | .NetworkSettings.Networks[].IPAddress')
echo $NAME $IPADDRESS
```
```text
/static-ip-container 172.20.128.2
```
alternatively (`docker-compose` uses the name of the directory as container name prefix):
```sh
CURRENT_DIR=$(pwd)
CURRENT_DIR=${CURRENT_DIR##*/}
CONTAINER_ID=$(docker container ls | grep $CURRENT_DIR|awk '{print $1}')
docker exec -it $CONTAINER_ID ifconfig eth0
```
this will produce
```sh
eth0      Link encap:Ethernet  HWaddr 02:42:AC:14:80:02
          inet addr:172.20.128.2  Bcast:172.20.255.255  Mask:255.255.0.0
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:2841 errors:0 dropped:0 overruns:0 frame:0
          TX packets:860 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:4038555 (3.8 MiB)  TX bytes:58275 (56.9 KiB)
```
```sh
dig +short 172.20.128.2
```
will show the ip address resoled from host machine (may be blank)

### Note
To set up sshd server on Alpine under Docker is a whole separate challenge and is unnecessary

### Cleanup
```sh
docker-compose stop
docker-compose rm -f
docker container rm -f $CONTAINER_ID
docker container prune -f
docker image rm -f basic-static-ip_static-ip-container
docker image prune -f
```
NOTE: some of the above commands appear duplicating one another

### Seee Also

  * https://stackoverflow.com/questions/27937185/assign-static-ip-to-docker-container
  * https://stackoverflow.com/questions/25529386/how-can-i-set-a-static-ip-address-in-a-docker-container
  * https://aws.amazon.com/blogs/opensource/demystifying-entrypoint-cmd-docker/
  * https://stackoverflow.com/questions/25775266/how-to-keep-docker-container-running-after-starting-services
  * https://jpetazzo.github.io/2014/06/23/docker-ssh-considered-evil/
  * https://wiki.alpinelinux.org/wiki/Setting_up_a_ssh-server
  * https://stackoverflow.com/questions/31297616/what-is-the-authoritative-list-of-docker-run-exit-codes

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

