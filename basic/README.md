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
mvn clean package
docker build -f Dockerfile -t basic-example . 
docker run -p 8086:8085 basic-example
```
test dockerized
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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
