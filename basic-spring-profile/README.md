### Info


* replica of [pavankumargaddam/spring-boot-profiles-example](https://github.com/pavankumargaddam/spring-boot-profiles-example)

### Usage

one can run spring boot application with specific active profile set explicitly.

#### Specify Active Profile via Commandline Argument

```sh
PROFILE=dev
mvn -Dspring.profiles.active=$PROFILE
```

#### Specify Profile In `application.properties`

Uncomment the specific line in `application.propertiels`:

```java
spring.profiles.active=dev,prod
```

```sh
curl -s http://localhost:2500/message
```
```text
Dev Environment  Running on Port Number 2500
```


### See Also

  * https://medium.com/@patryk.sosinski/mastering-spring-boot-profiles-in-application-properties-c4e9ea46e994
  * https://www.baeldung.com/properties-with-spring
  * https://docs.spring.io/spring-boot/docs/2.1.13.RELEASE/reference/html/boot-features-external-config.html
  * https://www.baeldung.com/configuration-properties-in-spring-boot


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
