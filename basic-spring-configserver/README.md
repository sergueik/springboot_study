### Info

this directory contains examples from [section 3](https://app.pluralsight.com/course-player?clipId=caf88459-8a71-4181-aaae-72dd78243410) __Simplifying Environment Management with Centralized Configuration__ of the training [Java Microservices with Spring Cloud: Developing Services](https://app.pluralsight.com/courses/36c015db-2983-4f97-8973-098b6a5d1fcc/table-of-contents), downgraded to much older release of Spring Boot/ Spring Cloud to avoid massive dependency pull

### Usage

verify code dependencies to be non-conflicting - important with ancient versions of spring boot starter used in this project

```sh
mvn test
```
* run
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```
* test
```sh
curl -s http://localhost:8888/app1/default | jq '.' -
```
to use the host ip find it via `ifconfig` or `hostname -i`
```sh
export HOST_IP=192.168.0.25
```
```sh
curl -s http://$HOST_IP:8888/app1/default
```
for external ip use [command](https://www.freecodecamp.org/news/bash-command-line-tips-to-help-you-work-faster/)
```sh
export EXTERNAL_IP=$(curl ifconfig.me)
```
on Windows host use the macro
```sh
export JQ=/c/tools/jq-win64.exe
```
```sh
curl -s http://localhost:8888/app1/default | $JQ '.' -
```

the result will be specific to the resource path e.g.:
```JSON
{
  "name": "app1",
  "profiles": [
    "default"
  ],
  "label": null,
  "version": null,
  "state": null,
  "propertySources": [
    {
      "name": "classpath:/config/app1.properties",
      "source": {
        "greeting": "hello"
      }
    }
  ]
}
```

### Client Test

* stop `configserver`
* start `config-client`
```sh
cd config-client
mvn spring-boot:run
```

* browse the page `http://192.168.0.25:8080/greeting`

```text
Config Server spring.config.import is: configserver:http://localhost:8888

Value is: dummy
```
* start `configserver`
* relaunch `config-client`
* reload thre page config-client.  Note the `value` change
```text
Config Server spring.config.import is: configserver:http://localhost:8888
```
```text
Value is: hello
```
change the 
```java
spring.application.name=app1
``` 
to
```java
spring.application.name=app1
```
and relaunch. Note the `value` change

```text
Value is: bonjour
```
* update the application name via Spring boot command line argument ():

```sh
mvn spring-boot:run -Dspring-boot.run.arguments="--spring.application.name=app3"
```
```text
Value is: hola
```
* similarly with java command
```sh
mvn -Dmaven.test.skip=true clean package
java -jar target/config-client-0.2.0-SNAPSHOT.jar --spring.application.name=app3
```

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-spring-configserver/screenshots/capture-greeting.png)

### Note


The spring-boot-starter-parent `2.3.4.RELEASE` release date was Sep 17, 2020
Tried to pick the matching `spring-cloud-dependencies`
First attempt was to build with version `2020.0.0` that was released on	Dec 22, 2020, but the two  were unable to work together

the `test` and `spring-boot:run` fail with
```text
09:17:55.587 [main] ERROR org.springframework.boot.SpringApplication - Application run failed
java.lang.NoClassDefFoundError: org/springframework/boot/Bootstrapper
```
- there was no explicit reference to the class in the sample code, the stack trace was all from the dependencies.

so switched to `Hoxton.SR8` - released on Aug 29, 2020 - confirmed that to work with  spring-boot-starter-parent `2.3.4.RELEASE`

### See Also

  * [quick Intro to Spring Cloud Configuration](https://www.baeldung.com/spring-cloud-configuration)
  * `@EnableConfigServer` annotation [documentation](https://cloud.spring.io/spring-cloud-config/multi/multi__spring_cloud_config_server.html) 
  * https://stackoverflow.com/questions/74658355/how-to-fix-java-lang-noclassdeffounderror-org-springframework-boot-bootstrapper
  * [External Configuration Data in Spring](https://springframework.guru/spring-external-configuration-data/)
  * (https://habr.com/ru/companies/otus/articles/576910/) (in Russian)
  * https://spring.io/projects/spring-cloud-config

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

