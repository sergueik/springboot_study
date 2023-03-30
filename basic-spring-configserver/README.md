### Info

this directory contains examples from [section 3](https://app.pluralsight.com/course-player?clipId=caf88459-8a71-4181-aaae-72dd78243410) __Simplifying Environment Management with Centralized Configuration__ of the training [Java Microservices with Spring Cloud: Developing Services](https://app.pluralsight.com/courses/36c015db-2983-4f97-8973-098b6a5d1fcc/table-of-contents), downgraded to much older release of Spring Boot/ Spring Cloud to avoid massive dependency pull

### Usage

verify code dependencies to be non-conflicting - important with ancient versions of spring boot starter used
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

* start `configserver`
* start config-client
```sh
cd config-client
mvn spring-boot:run
```
* browse the page

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-spring-configserver/screenshots/capture-greeting.png)

### Note


The spring-boot-starter-parent `2.3.4.RELEASE` release date was Sep 17, 2020
Tried to pick the matching `spring-cloud-dependencies`
First attempt was to build with version `2020.0.0` that was released on	Dec 22, 2020, but the two  were unable to work together

the `test` and `spring-boot:run` fail with
```text
09:17:55.587 [main] ERROR org.springframework.boot.SpringApplication - Applicati
on run failed
java.lang.NoClassDefFoundError: org/springframework/boot/Bootstrapper
```
- there was no explicit reference to the class in the sample code, the stack trace was all from the dependencies.

so switched to `Hoxton.SR8` - released on Aug 29, 2020 - confirmed that to work with  spring-boot-starter-parent `2.3.4.RELEASE`

### See Also
  * [quick Intro to Spring Cloud Configuration](https://www.baeldung.com/spring-cloud-configuration)
  * https://stackoverflow.com/questions/74658355/how-to-fix-java-lang-noclassdeffounderror-org-springframework-boot-bootstrapper
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
