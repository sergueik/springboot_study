### Info

example code from [document on layered buildpacks](https://spring.io/blog/2020/01/27/creating-docker-images-with-spring-boot-2-3-0-m1)

### Usage

* download a dummy project from initializr

```sh
VERSION=2.3.4.RELEASE
curl https://start.spring.io/starter.zip -d bootVersion=$VERSION -d dependencies=web -o /tmp/demo.zip
unzip /tmp/demo.zip
rm -fr mvnw mvnw.cmd HELP.md
```
* containerize using maven

```sh
mvn spring-boot:build-image
```
```sh
docker image ls | grep buildimage
```
```text
buildimage                         0.1.0-SNAPSHOT          357fb481ea71   41 years ago    225MB
```
* run
```sh
docker run -d -p 8080:8080  buildimage:0.1.0-SNAPSHOT
```
* test
```sh
curl -v -X POST http://localhost:8080/test
```
```text
< HTTP/1.1 200 
< Content-Length: 0
< HTTP/1.1 200 
< Content-Length: 0
```

TODO: add configuration to the `pom.xml` to avoid pulling the ubuntu as a base image.

### Without Maven

the standard scenario
```sh
export IMAGE=basic-layered
mvn clean package
docker build -f Dockerfile -t $IMAGE .
```
will avoid downloading the image `adoptopenjdk:11-jre-hotspot` as `builder`

```sh
docker run -p 8080:8080 -d $IMAGE
```
* check log for errors
```sh
docker logs $(docker container ls  -a | grep $IMAGE | awk '{print $1}')
```
### Troubleshoot Errors

when copying of the exploded java directories fails for some reason, the error is
```text
Error: Could not find or load main class org.springframework.boot.loader.JarLauncher
```
and
```text
[ERROR] Failed to execute goal org.springframework.boot:spring-boot-maven-plugin:2.3.4.RELEASE:repackage (repackage) on project buildimage: Execution repackage of goal org.springframework.boot:spring-boot-maven-plugin:2.3.4.RELEASE:repackage failed: Unable to find main class -> [Help 1]
```
to investigate build the `builder` image permanently 
```sh
docker build -f Dockerfile.builder -t buider .
```
and connect to it

### Timings
---|---
clear run | 5+ min
fully timed clear run, including image downloads | 15+ min
rerun with no changes  |  50.255 s
rebuild modified  | 01:17 min

### Cleanup

```sh
docker image rm $(docker image ls | grep buildimage | awk '{print $3}')
docker image rm $(docker image ls | grep paketobuildpacks | awk '{print $3}')
docker stop $(docker container ls | grep $IMAGE | awk '{print $1}' )
docker container rm  $(docker container ls -a | grep $IMAGE | awk '{print $1}' )
docker image rm $IMAGE
docker image prune -f
```

### See Also

  * https://www.baeldung.com/spring-boot-docker-images
  * https://www.baeldung.com/spring-boot-main-class

