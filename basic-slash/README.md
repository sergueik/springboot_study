### Info

This is environment to test the [URL Matching in Spring Boot 3](https://www.baeldung.com/spring-boot-3-url-matching)

### Usage

* pull images

```sh
docker pull maven:3.8.3-openjdk-17
```
```text
maven            3.8.3-openjdk-17        0b9ddcb8259e   12 months ago   785MB
```

```sh
IMAGE=sample-java17-app-build
docker build -f Dockerfile -t $IMAGE .
```
```sh
NAME=sample-java17-app

docker run --rm --name $NAME --user app --volume $(pwd):/app $IMAGE mvn clean -Dslash=true test
```
```text

INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 1.063 s - in com.example.demo.ApplicationTests
[INFO] 
[INFO] Results:
[INFO] 
[ERROR] Failures: 
[ERROR]   AcceptanceTest.test2:55 Unexpected exception type thrown, expected: <org.springframework.web.client.HttpClientErrorException> but was: <java.lang.AssertionError>
[INFO] 
[ERROR] Tests run: 4, Failures: 1, Errors: 0, Skipped: 1
[INFO] 

```
```sh
NAME=sample-java17-app
docker run --rm --name $NAME --user app --volume $(pwd):/app $IMAGE mvn clean test
```
```text
INFO] Results:
[INFO] 
[WARNING] Tests run: 4, Failures: 0, Errors: 0, Skipped: 1
[INFO] 
```
### See Also
  * https://stackoverflow.com/questions/11560916/spring-mvc-3-1-how-to-map-urls-with-a-trailing-slash
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
