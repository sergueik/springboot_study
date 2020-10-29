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
### Alternative log4j2 Configurations

There are two property file (combining those is still work in progress):

```sh
log4j2.properties.CONSOLE-ONLY
log4j2.properties.FILE-ONLY
```
remove the and rename one of the above to simply `log4j2.properties` to see it work

### Testing in Docker Container
```sh
mvn clean package
```
```sh
IMAGE=basic-log4j2
docker build -t $IMAGE -f Dockerfile .
```
```sh
rm logs/*
docker run -d -p 8080:8080 -v $(pwd)/logs:/work/logs:rw $IMAGE
```
* inspect logs locally
```sh
ls -l logs/
total 12
-rw-rw-r-- 1 sergueik systemd-journal  183 Oct 29 01:19 App.1.log.gz
-rw-rw-r-- 1 sergueik systemd-journal  958 Oct 29 01:19 App.log
-rw-r--r-- 1 sergueik systemd-journal 2063 Oct 29 01:19 Common.log
```
* inspect logs in container
```sh
docker exec -w /work -it $(docker container ls | grep $IMAGE | awk '{print $1}' ) sh -c 'ls -l '
```
```sh
-rw-rw-r--    1 root     root      20946246 Oct 29 00:10 app.jar
drwxrwxr-x    2 myuser   1000          4096 Oct 29 00:19 logs

```
Note:
```sh
docker container ls | grep $IMAGE |  awk '{print $1}' | xargs -IX  docker exec -w '/work' -it X sh
```
fails with
```sh
the input device is not a TTY
```
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

