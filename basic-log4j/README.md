### Info

This project contains [minimal demo code of log4j example]
### Usage

#### Testing Locally

* run standalone app
```sh
mvn compile
cp src/main/resources/log4j.xml .
java -Dlog4j.configuration=log4j.xml -cp $HOME/.m2/repository/log4j/log4j/1.2.17/log4j-1.2.17.jar:target/classes example.Basic
```
* run standalone spring boot app
```sh
mvn -Dlog4j.configuration=log4.xml spring-boot:run
```
and check the messages in `logs/App.log` and to console:
```sh
21:35:06.166 [main] INFO  o.s.b.c.e.t.TomcatEmbeddedServletContainer - Tomcat st
arted on port(s): 8080 (http)
```
and
```sh
[main] INFO  22ogger - init message
[main] INFO  22ogger - init message
[main] WARN  23ogger - init message
```
in `App.log` only
then
```sh
curl http://localhost:8080/example?data=12345
```
this will be logged:
```sh
```
### Adding Rabbitmq
* run standalone basic example
```sh
mvn compile

java -Dlog4j.configuration=log4j.xml -cp $HOME/.m2/repository/log4j/log4j/1.2.17/log4j-1.2.17.jar:src/main/resources/example.rabbitmq-appender-0.1.0-SNAPSHOT.jar:$HOME/.m2/repository/com/rabbitmq/amqp-client/5.8.0/amqp-client-5.8.0.jar:$HOME/.m2/repository/org/slf4j/slf4j-log4j12/1.7.5/slf4j-log4j12-1.7.5.jar:$HOME/.m2/repository/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar:$HOME/.m2/repository/org/json/json/20160810/json-20160810.jar:target/classes example.Basic
```
### See Also
 * [Logging in Spring Boot](https://www.baeldung.com/spring-boot-logging)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

