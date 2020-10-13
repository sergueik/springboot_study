### Info

This directory contains replica of example __Building an Optimized Container Image for a Spring Boot Application with Docker__ from
the article [Creating Optimized Docker Images for a Spring Boot Application](https://reflectoring.io/springi-boot-docker/), converted to three step `Dockerfile`
using the [amond/openjdk:11-jdk-alpine] and [azul/zulu-openjdk-alpine:11](https://hub.docker.com/r/azul/zulu-openjdk-alpine/tags) images
and __docker-alpine-java-maven__ [Dockerfile](https://github.com/timbru31/docker-alpine-java-maven/blob/master/Dockerfile)
to compile the jar, extract layers and run launcher class `org.springframework.boot.loader.JarLauncher`
to avoid having to install the JDK 11 in the host.

There is no real need to run the application on Java 11 for this.

### Usage
#### Three Step
* compile and package jar on JDK11 and build Docker image with JDK 11 and maven,in three step
```sh
MAVEN_IMAGE=alpine-java11-maven
docker build -t $MAVEN_IMAGE -f Dockerfile.$MAVEN_IMAGE .
```
* build image with layered `application` directory

```sh
IMAGE=layered-springboot
CONTAINER='layered-springboot-container'
docker build -t $IMAGE -f Dockerfile.3step .
```
* run the container

```sh
docker run -it -p 8085:8085 --name $CONTAINER $IMAGE
```
* verify
```sh
curl http://localhost:8085/basic
```
* inspect
```sh
docker exec -it $CONTAINER sh -c "find /application -type d"
```
this will show
```sh
/application
/application/BOOT-INF
/application/BOOT-INF/lib
/application/BOOT-INF/classes
/application/BOOT-INF/classes/example
/application/META-INF
/application/META-INF/maven
/application/META-INF/maven/example
/application/META-INF/maven/example/basic
/application/org
/application/org/springframework
/application/org/springframework/boot
/application/org/springframework/boot/loader
/application/org/springframework/boot/loader/jar
/application/org/springframework/boot/loader/archive
/application/org/springframework/boot/loader/data
/application/org/springframework/boot/loader/util
/application/org/springframework/boot/loader/jarmode
```
* stop
```sh
docker stop $CONTAINER
docker container rm $CONTAINER
```
#### Two Step
NOTE: one can pefroem layer extraction and run the layered springboot app on Docker image __amond/openjdk:11-jdk-alpine__  hosting Java 11
or Java 8 ones: __openjdk:8-jdk-alpine3.9__, __openjdk:8-jre-alpine3.9__.

* compile and package jar on defaault JDK on host (e.g. JDK 8)
```sh
mvn -Dmaven.test.skip=true clean package
```
* build Docker image with JDK 11,in two step: extracting layered `application` directory

```sh
IMAGE=layered-springboot
CONTAINER='layered-springboot-container'
docker build -t $IMAGE -f Dockerfile.2step .
```
* run the container

```sh
docker run -it -p 8085:8085 --name $CONTAINER $IMAGE
```
this will eventually show
```sh
Started ExampleApplication
```
* verify
```sh
curl http://localhost:8085/basic
```
this will respond with
```sh
Hello basic
```
* inspect
```sh
docker exec -it $CONTAINER sh -c "find /application -type d"
```
this will show
```sh
/application
/application/BOOT-INF
/application/BOOT-INF/lib
/application/BOOT-INF/classes
/application/BOOT-INF/classes/example
/application/META-INF
/application/META-INF/maven
/application/META-INF/maven/example
/application/META-INF/maven/example/basic
er run -it -p 8085:8085 --name $CONTAINER $IMAGE
/application/org/springframework/boot
/application/org/springframework/boot/loader
/application/org/springframework/boot/loader/jar
/application/org/springframework/boot/loader/archive
/application/org/springframework/boot/loader/data
/application/org/springframework/boot/loader/util
/application/org/springframework/boot/loader/jarmode
```

* stop
```sh
docker stop $CONTAINER
docker container rm $CONTAINER
```
### Cleanup
```sh
docker stop $CONTAINER
docker container prune -f
docker image rm $IMAGE
docker image prune -f
```
NOTE: this exercise apparenlty requires both JDK 11 and SpringBoot parent __2.3.x__.


### See Also

 * [Creating Optimized Docker Images for a Spring Boot Application](https://reflectoring.io/spring-boot-docker/)
 * [Создание оптимизированных образов Docker для приложения Spring Boot](https://habr.com/ru/post/522122/) (Russian transation)
 * [Docker/Maven project producing Optimized Docker Images for Spring Boot example](https://github.com/thombergs/code-examples/tree/master/spring-boot/spring-boot-docker)
 * https://stackoverflow.com/questions/27767264/how-to-dockerize-maven-project-and-how-many-ways-to-accomplish-it
 * Docker image [dive tool](https://github.com/wagoodman/dive) (writte in go) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
