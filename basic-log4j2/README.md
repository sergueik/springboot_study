### Info

This project contains [minimal demo code of log4j2 example]
### Usage

#### Testing Springboot App locally
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
[main] INFO  22ogger - init message
[main] INFO  22ogger - init message
[main] WARN  23ogger - init message
```
in `App.log` only
then
```sh
for cnt in $(seq 0 1 3); do curl "http://localhost:8080/example?data='${cnt}+test'"; done
```
and check the appearance of new messages in `App.log` and console:
```sh
[http-nio-8080-exec-10] INFO  20ogger - raw data '0 test'
[http-nio-8080-exec-10] INFO  20ogger - raw data '0 test'
[http-nio-8080-exec-10] INFO  20ogger - handler received: '0 test'
[http-nio-8080-exec-10] INFO  20ogger - handler received: '0 test'
[http-nio-8080-exec-9] INFO  20ogger - raw data '1 test'
[http-nio-8080-exec-9] INFO  20ogger - raw data '1 test'
[http-nio-8080-exec-9] INFO  20ogger - handler received: '1 test'
[http-nio-8080-exec-9] INFO  20ogger - handler received: '1 test'
[http-nio-8080-exec-8] INFO  20ogger - raw data '2 test'
[http-nio-8080-exec-8] INFO  20ogger - raw data '2 test'
[http-nio-8080-exec-8] INFO  20ogger - handler received: '2 test'
[http-nio-8080-exec-8] INFO  20ogger - handler received: '2 test'
[http-nio-8080-exec-7] INFO  20ogger - raw data '3 test'
[http-nio-8080-exec-7] INFO  20ogger - raw data '3 test'
[http-nio-8080-exec-7] INFO  20ogger - handler received: '3 test'
[http-nio-8080-exec-7] INFO  20ogger - handler received: '3 test'
```

### Testing in Docker Container
TBD

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

