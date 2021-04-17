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
docker container prune -f
docker image prune -f
```

### See Also
  * [step by step](https://github.com/in28minutes/SpringBootWebApplicationStepByStep) Web Application with Spring Boot
  * [package springboot as standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * [Test Strategies around Spring Boot](https://github.com/mechero/spring-boot-testing-strategies)
  * [REST Spring boot Unit tests](https://github.com/bytestree/spring-restful-service-unit-test)
  * Docker [command list](https://habr.com/ru/company/flant/blog/336654/) (in Russian)
  * deal with [failing ipv6](https://stackoverflow.com/questions/30750271/disable-ip-v6-in-docker-container) in Docker
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


 listen tcp6 [::]:8086: socket: address family not supported by protocol.
 Docker version 20.10.6, build 370c289
https://medium.com/@gauravsj9/how-to-install-specific-docker-version-on-linux-machine-d0ec2d4095
