### Info

This directory contains basic project to docker-package the application jars via [jib plugin](https://github.com/GoogleContainerTools/jib/tree/master/jib-maven-plugin).
This is handy when the Docker hosting machine does not jave Java environment,
and development is taking place on a separate host, e.g. on a Windows 7 machine which is not configured to run docker daemon

### Usage

```sh
mvn clean compile jib:dockerBuild
```
or simply
```sh
mvn clean package
```

then
```sh
USER=sergueik
DOCKER_HOST=192.168.0.64
```
```sh
scp target/jib-image.tar $USER@$DOCKER_HOST:Downloads
```
```
and
```sh
ssh $USER@$DOCKER_HOST docker load --input Downloads/jib-image.tar
```
this will reply
```
Loaded image: jib:0.1.0-SNAPSHOT
```
```sh
ssh $USER@$DOCKER_HOST docker image ls -a jib
```
this will reply
```sh
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
jib                 0.1.0-SNAPSHOT      4b5522dfd287        50 years ago        99.3MB
```
### Running the Container

```sh
docker container run -it -p 8085:8085 -t jib:0.1.0-SNAPSHOT
```
```sh
curl http://$DOCKER_HOST:8085/basic
```

will respond with
```sh
Hello basic
```

### Debugging the Container

The image built by __jib__ has some default configuration which may not be what the developer wanted

```sh
docker exec -it $(docker container ls | grep 'jib' | awk '{print $1}') sh
```
then in the container find how the Springboot app is actually run:
```sh
ps ax |  grep jav[a]
    1 root      0:15 java -cp /app/resources:/app/classes:/app/libs/* example.ExampleApplication
```
it will have the jar:
```sh
ls /opt/example.jib.jar
```
but it will not not be running it for thr service.

### Cleanup

```sh
docker container prune -f
docker image rm $(docker image ls -a jib| tail -1 |awk '{print $3}')
docker image prune -f
```

### See Also
  * https://www.baeldung.com/jib-dockerizing
  * https://github.com/GoogleContainerTools/jib/blob/master/docs/faq.md

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
