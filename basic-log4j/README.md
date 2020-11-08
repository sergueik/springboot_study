### Info

This project contains minimal demo code of __log4j__ logging

### Usage

#### Testing Locally
* run standalone app
```sh
mvn compile
cp src/main/resources/log4j.xml .
java -Dlog4j.configuration=log4j.xml -cp /home/sergueik/.m2/repository/log4j/log4j/1.2.17/log4j-1.2.17.jar:target/classes example.Basic
```
* run standalone spring boot app
```sh
mvn -Dlog4j.configuration=log4j.xml spring-boot:run
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
INFO  LogHelper - INFO: raw data: 123
INFO  LogHelper - INFO: handler received: 123
```
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

