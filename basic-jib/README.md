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
ssh $USER@$DOCKER_HOST docker load --input /home/sergueik/Downloads/jib-image.tar
```
this will reply
```
Loaded image: jib:0.1.0-SNAPSHOT
```
```sh
ssh $USER@$DOCKER_HOST docker image ls  -a jib
```
this will reply
```sh
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
jib                 0.1.0-SNAPSHOT      4b5522dfd287        50 years ago        99.3MB
```

### Cleanup

```sh
docker image rm $(docker image ls -a jib| tail -1 |awk '{print $3}')
```
### See Also
  * https://www.baeldung.com/jib-dockerizing
  * https://github.com/GoogleContainerTools/jib/blob/master/docs/faq.md

