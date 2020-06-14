

### Info
https://habr.com/ru/post/505720/
https://github.com/layonez/kafka-example

# https://github.com/wurstmeister/kafka-docker

# https://hub.docker.com/r/jplock/zookeeper/dockerfile/
# https://github.com/danielguerra69/alpine-kafka

### See Also
  * https://github.com/rawmind0/alpine-kafka
  * # https://github.com/solsson/dockerfiles


```sh
export IMAGE=kafka
export NAME='example-kafka'
docker build -t $IMAGE -f Dockerfile.kafka .

docker run  -v /var/run/docker.sock:/var/run/docker.sock -e HOST_IP=$1 -e ZK=$2 -it $IMAGE /bin/bash
```
```sh
docker run --rm -v /var/run/docker.sock:/var/run/docker.sock -e HOST_IP=$1 -e ZK=$2 -it $IMAGE /bin/bash
```


### TODO

* Cache the download `kafka_2.12-2.5.0.tgz` on the host
* update path to include to handle the error: `docker: Error response from daemon: OCI runtime create failed: container_linux.go:349: starting container process caused "exec: \"java\": executable file not found in $PATH": unknown.`o

```
IMAGE_NAME=openjdk
IMAGE_TAG='8-jre-alpine3.9'
ID=$(docker image ls | grep $IMAGE_TAG |awk '{print $3}')
docker run  -t $ID
```
```sh
docker image rm -f $ID
docker pull $IMAGE_NAME:$IMAGE_TAG
```
