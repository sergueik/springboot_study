### Info

This directory contins a replica of the Java native [socket library](https://github.com/jnr/jnr-unixsocket)
project test code
added as a maven dependency [maven repository](https://mvnrepository.com/artifact/com.github.jnr/jnr-unixsocket)

It is used with Docker basic image - the build  exercises socket sharing.
### Testing
run java as test
```sh
export EXTERNAL_CLIENT=true
mvn test
```
```sh
Running example.BasicFunctionalityTest
Opened socket: /tmp/xxx/test62.sock
Sleep: 120000 m/sec
Processed: message
```
connect to socket (use the socket name from the test console output)
```sh
echo -e "message\n" | socat unix-connect:/tmp/xxx/test62.sock STDIO
```
```text
message
```

### Testing within Docker container
```sh
mvn clean package
```

```sh
java -cp target/example.basic-unixsocket-jar-with-dependencies.jar example.BasicSocketServe
```
will print the path to the socket to the console.

```sh
echo -e "message\n" | socat unix-connect:/tmp/xxx/test40.sock STDIO
message
```

plase into the container
```sh
export IMAGE='basic-socket'
export NAME='example-socket'
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -d $IMAGE
```

check logs:
```sh
export CONTAINER=$(docker container ls -a| grep $NAME| awk '{print $1}')
docker logs $CONTAINER
```
this wll show the name of the file:
```sh
Opened socket: /tmp/test99.sock
```
verify on the container
```sh
docker exec -it  $CONTAINER sh
```
there
```sh
apk add socat
echo -e "message\n" | socat unix-connect:/tmp/test99.sock STDIO 
```
this will accomplish the IPC.
```sh
message
```
Note: current version of `BasicServer` has an incorrect event processing logic and 
does not handle messages beyong the very first one.

mount the volume across

```sh
mkdir /tmp/xxx
rm -f /tmp/xxx/*

export IMAGE='basic-socket'
export NAME='example-socket'
docker build -t $IMAGE -f Dockerfile .

docker run --name $NAME --volume "/tmp/xxx/:/tmp/xxx/:rw" -d $IMAGE
```
to avoid the ownership tweaks one can simply interact with socket file in mounted volume as root:

```sh
sudo -s
```
```sh
echo -e "message\n" | socat unix-connect:/tmp/xxx/test26.sock STDIO
```
thiis will respond with
```sh
message
```
### Cleanup

```sh
docker container stop $CONTAINER;
docker container prune -f ;
docker image rm $IMAGE;
docker image prune -f ;
```

### See  Also

 * basics of [user id changes](https://www.jujens.eu/posts/en/2017/Feb/15/docker-unix-socket/) to make socket file available on both host and container(s)
 * Java socket library [source](https://github.com/mcfunley/juds)
 * Java socket library [maven repo](https://mvnrepository.com/artifact/uk.co.caprica/juds)
 * another Java [socket library](https://github.com/jnr/jnr-unixsocket) and [maven repository](https://mvnrepository.com/artifact/com.github.jnr/jnr-unixsocket)
 * yet another Java [socket library](https://github.com/kohlschutter/junixsocket) and [maven artifacts](https://mvnrepository.com/artifact/com.kohlschutter.junixsocket)
 * anothed explanation of constructing and using [unix socket in the container](https://medium.com/better-programming/about-var-run-docker-sock-3bfd276e12fd)
 * https://superuser.com/questions/834307/can-curl-send-requests-t:qo-sockets 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


