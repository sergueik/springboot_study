### Info

Springboot Docker basic project extracted from [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted to run on alpine openjdk jre base image.
### Test

* run locally
```sh
mvn clean spring-boot:run
```
* test locally
```sh
curl http://localhost:8085/basic
Hello basic
```
* run in container
```sh
IMAGE=basic-example
mvn clean package
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
docker run -p 8086:8085 $IMAGE
```
or
```sh
docker run -p $(hostname -i):8086:8085 $IMAGE
```
* NOTE: the `$(hostname -i):` argument was added as workaround of forced ipv6 switch
```sh
Error starting userland proxy: listen tcp6 [::]:8086:
socket: address family not supported by protocol
```
observed in Docker version __20.10.6__ on a host where ipv6 was [turned off](https://linuxconfig.org/how-to-disable-ipv6-address-on-ubuntu-18-04-bionic-beaver-linux)
* to also mount

```sh
CONTAINER=$(docker container ls | grep $IMAGE | awk '{print $1}')
DESTINATION=$(docker inspect $CONTAINER | jq -cr '.[]|.Mounts|.[]|.Destination')
docker exec -it $CONTAINER ls $DESTINATION
```
* test dockerized
```sh
curl http://localhost:8086/basic
Hello basic
```
destroy all started containers and image afterwards
```sh
docker container stop $(docker container ls |  grep $IMAGE | awk '{print $1}')
docker container prune -f
docker image prune -f
```
### Remote debugging

use `Dockerfile.DEBUG` instead of plain `Dockerfile` to pass the [debugging server arguments](https://dzone.com/articles/how-debug-remote-java-applicat) to `ENTTRYPOINT` and map the debugger port (currently hardcoded to __8998__) as:
```sh
docker build -f Dockerfile.DEBUG -t $IMAGE .
docker run -p 8085:8085  -p 8998:8998 $IMAGE
```
in the eclipse remote application debug configuration, use the IP address of the host
the launch configuration `.basic.launch` from `$HOME\workspace\.metadata\.plugins\org.eclipse.debug.core\.launches` looks like this:
```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<launchConfiguration type="org.eclipse.jdt.launching.remoteJavaApplication">
<listAttribute key="org.eclipse.debug.core.MAPPED_RESOURCE_PATHS">
<listEntry value="/basic"/>
</listAttribute>
<listAttribute key="org.eclipse.debug.core.MAPPED_RESOURCE_TYPES">
<listEntry value="4"/>
</listAttribute>
<booleanAttribute key="org.eclipse.jdt.launching.ALLOW_TERMINATE" value="true"/>
<mapAttribute key="org.eclipse.jdt.launching.CONNECT_MAP">
<mapEntry key="hostname" value="192.168.0.64"/>
<mapEntry key="port" value="8998"/>
</mapAttribute>
<stringAttribute key="org.eclipse.jdt.launching.PROJECT_ATTR" value="basic"/>
<stringAttribute key="org.eclipse.jdt.launching.VM_CONNECTOR_ID" value="org.eclipse.jdt.launching.socketAttachConnector"/>
</launchConfiguration>

```

### See Also

  * [step by step](https://github.com/in28minutes/SpringBootWebApplicationStepByStep) Web Application with Spring Boot
  * [package springboot as standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * [Test Strategies around Spring Boot](https://github.com/mechero/spring-boot-testing-strategies)
  * [REST Spring boot Unit tests](https://github.com/bytestree/spring-restful-service-unit-test)
  * Docker [command list](https://habr.com/ru/company/flant/blog/336654/) (in Russian)
  * deal with [failing ipv6](https://stackoverflow.com/questions/30750271/disable-ip-v6-in-docker-container) in Docker
  * [using Docker environment variables in build phase](https://vsupalov.com/docker-build-pass-environment-variables/)
  * [Docker commands](https://habr.com/ru/company/ruvds/blog/440660/) (in Russian)
  * official __Spring Boot Docker__ [documentation](https://spring.io/guides/topicals/spring-boot-docker/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
