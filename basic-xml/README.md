### Info

Springboot Docker classic app-config.xml with commandline argument exercise project based on [Java Annotation and XML Bean Configuration](https://www.concretepage.com/spring-boot/spring-boot-xml-configuration-example) article

### Standalone test

```sh
mvn -Dtitle="Java Spring" -Dcategory="Programming" spring-boot:run
```
observe in  console:
```sh
Article: Programming "Java Spring"
```
access via GET request:
```sh
curl http://localhost:8080/user/articles
```
```json
[
  {
    "title" : "Java Spring",
    "category" : "Programming"
  }
]
```
### Dockerized Test

* package the jar
```sh
mvn -Dmaven.test.skip=true clean package
```
* build the image (note somewhat nonstandard way of dealing with white space in values):
```sh
docker build -f Dockerfile -t basic-xml-example --build-arg "title=Docker Containers" --build-arg "category=Programming" .
```
* inspect the image
```sh
docker image inspect basic-xml-example | jq '.[].ContainerConfig.Cmd'
```
``` sh
[

  "/bin/sh",
  "-c",
  "#(nop) ",
 "ENTRYPOINT [\"java\" \"-Dtitle=${title_env}\" \"-Dcategory=${category_env}\" \"-jar\" \"app.jar\"]"]
```


```sh
docker image inspect basic-xml-example | jq '.[].ContainerConfig.Env'
```
```sh
[
  "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/lib/jvm/java-1.8-openjdk/jre/bin:/usr/lib/jvm/java-1.8-openjdk/bin",
  "LANG=C.UTF-8",
  "JAVA_HOME=/usr/lib/jvm/java-1.8-openjdk/jre",
  "JAVA_VERSION=8u212",
  "JAVA_ALPINE_VERSION=8.212.04-r0",
  "title_env=Docker Containers",
  "category_env=Programming"
]
```
* run docker container

```sh
docker run --rm -p 8080:8080 basic-xml-example
```

* in console observe the debugging message coming from Java `ArticleService` class instance and listing the injected params:
```java
@SpringBootApplication
@ImportResource("classpath:app-config.xml")
public class Launcher {
  public static void main(String[] args) {
    ApplicationContext applicationContext = SpringApplication.run(Launcher.class, args);
   ArticleService articleService = applicationContext.getBean(ArticleService.class);
   System.err.println(articleService.processMsg());

```
```sh
Started Launcher in 13.282 seconds (JVM running for 27.857)
Article: Programming "Docker Containers"
```
repeat the web access test
```sh
curl http://localhost:8080/user/articles
```
```json
[ 
  {
    "title" : "Docker Containers",
    "category" : "Programming"
  }
]
```
### Runtime Argument test
build 
```sh
docker build -f Dockerfile.noargs -t basic-args-example  .
```
run
```sh
docker run -it  -p 8080:8080 basic-xml-example _ -Dtitle="Bash scripting" -Dcategory="Unix Administration"
```
```
* verify
```sh
curl http://localhost:8080/user/articles
```
```json
[ {
  "title" : "Bash scripting",
  "category" : "Administration"
} ]
```

* stop container from another terminal
```sh
ID=$(docker container list --filter "ancestor=basic-xml-example"  --format {{.ID}})
docker container stop $ID
```
```sh
3ecbc9003922
```
* clean up
```sh
docker image prune -f
docker image rm basic-xml-example
```

### See Also:

  * [Properties with Spring and Spring Boot](https://www.baeldung.com/properties-with-spring)
  * [Constructor Dependency Injection in Spring](https://www.baeldung.com/constructor-injection-in-spring)
  * [Docker command reference](https://docs.docker.com/engine/reference/commandline/ps/)
