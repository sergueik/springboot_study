### Info

This directory contains a replica of the project
[markhobson/maven-chrome](https://github.com/markhobson/docker-maven-chrome)

### Usage

Using [original](https://hub.docker.com/r/markhobson/maven-chrome/) Dockerfile, one may compile and test the sample project interactively:

```sh
IMAGE=basic-maven-jdk
CONTAINER=basic-chrome-maven
docker build -t $IMAGE -f Dockerfile.ORIGINAL .
docker run --name $CONTAINER -it $IMAGE sh
```
open a second terminal
```sh
docker cp demo $CONTAINER:/demo
```
switch to the shell running in the container
```sh
cd /demo
mvn verify package
mvn test
```
alternatively use the mapped source directory
```sh
docker run --rm -it -v "$PWD":/usr/src -w /usr/src $IMAGE mvn clean verify test
```
### See Also

https://groups.google.com/forum/?nomobile=true#!topic/selenium-users/c7r1nsqCB8U
