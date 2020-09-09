### Info

This project contains [minimal demo code of logback example](http://logback.qos.ch/manual/appenders.html) converted to a regular springboot application logging initialization and operation

### Usage

* test application locally
```sh
mvn clean package
java -jar target/example.logback.jar
```

__Note__: it does not appear that logback's `RollingFileAppender` [class](https://github.com/qos-ch/logback/blob/master/logback-core/src/main/java/ch/qos/logback/core/rolling/RollingFileAppender.java)
supports configuring log file permissions.

__Note__: launching the class with classpath
```
java -cp target/example.logback.jar:target/lib/*:target/conf example.Example
```
or
```cmd
java -cp target\example.logback.jar;target\lib\*;target\conf example.Example
```
does not appear to work:
```java
Error: Could not find or load main class example.Example
```

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
for cnt in $(seq 10 1 20); do curl http://localhost:8080/example?data=$cnt; done
```
and check the appearance of new messages in `App.log`:
```sh
41629 [http-nio-8080-exec-1] INFO  example.Example - exampleHandler received: 18
41629 [http-nio-8080-exec-1] WARN  example.Example - exampleHandler received: 19
41629 [http-nio-8080-exec-1] DEBUG example.Example - exampleHandler received: 20
```

### Testing in Docker Container
```sh
mvn clean package
docker container prune -f
docker image rm basic-logback

docker build -f Dockerfile -t basic-logback .
mkdir logs;
NAME='basic-logback-container'
docker run --name $NAME -v $(pwd)/logs:/work/logs:rw -p 8080:8080 basic-logback
```
to verify
```sh
docker exec -it $NAME sh
```
### See Also

 * https://www.codingame.com/playgrounds/4497/configuring-logback-with-spring-boot
 * https://stackoverflow.com/questions/2602415/rolling-logback-logs-on-filesize-and-time
 * https://www.baeldung.com/java-logging-rolling-file-appenders

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

