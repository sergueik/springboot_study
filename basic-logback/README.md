### Info

This project contains [minimal demo code of logback example](http://logback.qos.ch/manual/appenders.html) converted to a regular springboot application logging initialization and operation

### Usage

* test application locally
```sh
mvn clean package
java -cp target\logback-0.0.1-SNAPSHOT.jar;target\lib\*;target\conf example.Example
```
Note: it does not appear that logback's `RollingFileAppender` [class](https://github.com/qos-ch/logback/blob/master/logback-core/src/main/java/ch/qos/logback/core/rolling/RollingFileAppender.java)
supports configuring log file permissions.
### Testing Springboot App 
```sh
mvn spring-boot:run
```
and check the messages in `App.log` and console:
```sh
21:35:06.166 [main] INFO  o.s.b.c.e.t.TomcatEmbeddedServletContainer - Tomcat st
arted on port(s): 8080 (http)
```
and
```sh
3175 [main] INFO  example.Example - init message
3175 [main] WARN  example.Example - init message
```
in `App.log` only
then
```sh
curl http://localhost:8080/example?data=42
```
and check the appearance of new messages in `App.log`:
```sh
41629 [http-nio-8080-exec-1] INFO  example.Example - exampleHandler received: 42
41629 [http-nio-8080-exec-1] WARN  example.Example - exampleHandler received: 42
41629 [http-nio-8080-exec-1] DEBUG example.Example - exampleHandler received: 42
```
### See Also

 * https://www.codingame.com/playgrounds/4497/configuring-logback-with-spring-boot

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

