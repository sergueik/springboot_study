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
docker run -p 8086:8085 $IMAGE
```
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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
