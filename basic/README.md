### Info

Springboot Docker basic project extracted from [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example)
### Test

* run locally
```sh
mvn clean spring-boot:run
```
* run in container
```sh
mvn clean package
docker build -f Dockerfile -t basic-example . 
docker run -p 8085:8085 basic-example
```
```sh
curl http://localhost:8085/basic
Hello basic
```
- need to manually destroy all started containers and image afterwards

### See Also
  * [package springboot as standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
