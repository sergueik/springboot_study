MAGE=kafka-standalone### Info

https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile
with kafka stack and zookeeper in one host
modified the version it pulls from [mirror](https://dlcdn.apache.org/kafka/)

#### usage

```sh
IMAGE=kafka-standalone
docker build -t $IMAGE -f Dockerfile .
```
```sh
export KAFKA_ADVERTISED_HOST_NAME=localhost
NAME=kafka-standalone
docker run -d --name $NAME -p 9092:9092 -e KAFKA_ADVERTISED_HOST_NAME=localhost -e KAFKA_CREATE_TOPICS="test-topic:1:1" $IMAGE
docker logs $NAME
```
eventualy see
```text
[2021-10-20 18:22:20,721] INFO Created log for partition test-topic-0 in /kafka/kafka-logs/d8af01b63460/test-topic-0 with properties {} (kafka.log.LogManager)
[2021-10-20 18:22:20,728] INFO [Partition test-topic-0 broker=1001] No checkpointed highwatermark is found for partition test-topic-0 (kafka.cluster.Partition)
[2021-10-20 18:22:20,732] INFO [Partition test-topic-0 broker=1001] Log loaded for partition test-topic-0 with initial high watermark 0 (kafka.cluster.Partition)
```

bootstrap-servers: ${kafka_bootstrap_servers:localhost:9092}
```sh
IMAGE=kafka-standalone
```

* run single kafka
```sh
NAME=kafka-standalone
docker run -d --name $NAME -p 9092:9092 -e KAFKA_ADVERTISED_HOST_NAME=localhost -e KAFKA_CREATE_TOPICS="test-topic:1:1" $IMAGE
```
* run zookeeper
```
NAME=zookeeper
docker run --name $NAME -d -p 2181:2181 $IMAGE zookeeper-server-start.sh config/zookeeper.properties
```
* run single kafka
```sh
NAME=kafka-1
IMAGE=kafka-standalone
docker run -d -v /var/run/docker.sock:/var/run/docker.sock -e KAFKA_ADVERTISED_HOST_NAME=localhost --link zookeeper -p 9092:9092 --name $NAME $IMAGE
```
* create topic

```sh
docker run --rm --link zookeeper $IMAGE kafka-topics.sh --create --zookeeper zookeeper:2181 --replication-factor 1 --partitions 1 --topic test-topic                           
```
this will print
```text
Created topic test-topic.
```
remove `--partition 1` argument

### OLD


Docker kafka standalone Alpine with producer and  consumer running on dev host, for basic API practice
based on https://github.com/wurstmeister/kafka-docker, needs a second docker instance to host zookeeper and traffic encryption
currently the https://github.com/wurstmeister/zookeeper-docker/blob/master/Dockerfile is based on Ubintu, willbe too big.

https://habr.com/ru/post/505720/
https://github.com/layonez/kafka-example - Java client

# https://github.com/wurstmeister/kafka-docker

# https://hub.docker.com/r/jplock/zookeeper/dockerfile/
# https://github.com/danielguerra69/alpine-kafka

### See Also
  * https://github.com/rawmind0/alpine-kafka
  * # https://github.com/solsson/dockerfiles

### Usage

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
docker run -t $ID
```
```sh
docker image rm -f $ID
docker pull $IMAGE_NAME:$IMAGE_TAG
```

### See Also

  * https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile - NOTE: uses alpine 3:10

### Info

https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile
with kafka stack and zookeeper in one host
modified the versio it pulls from [mirror](https://dlcdn.apache.org/kafka/)
Docker kafka standalone Alpine with producer and  consumer running on dev host, for basic API practice
based on https://github.com/wurstmeister/kafka-docker, needs a second docker instance to host zookeeper and traffic encryption
currently the https://github.com/wurstmeister/zookeeper-docker/blob/master/Dockerfile is based on Ubintu, willbe too big.

https://habr.com/ru/post/505720/
https://github.com/layonez/kafka-example - Java client

# https://github.com/wurstmeister/kafka-docker

# https://hub.docker.com/r/jplock/zookeeper/dockerfile/
# https://github.com/danielguerra69/alpine-kafka

### See Also
  * https://github.com/rawmind0/alpine-kafka
  * # https://github.com/solsson/dockerfiles

### Usage

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
docker run -t $ID
```
```sh
docker image rm -f $ID
docker pull $IMAGE_NAME:$IMAGE_TAG
```

### See Also

  * https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile - NOTE: uses alpine 3:10


