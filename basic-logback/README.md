### Info

This project contains [minimal demo code of logback example](http://logback.qos.ch/manual/appenders.html) converted to a regular springboot application logging initialization and operation

### Usage

* test application locally
```sh
mvn clean package
java -jar target/example.logback.jar
curl -vk 127.0.0.1:8080/example
```

It does not appear that logback's `RollingFileAppender` [class](https://github.com/qos-ch/logback/blob/master/logback-core/src/main/java/ch/qos/logback/core/rolling/RollingFileAppender.java)
supports configuring log file permissions.

__NOTE__: launching the class with classpath
```
java -cp target/example.logback.jar:target/lib/*:target/conf example.Example
```
or
```cmd
java -cp target\example.logback.jar;target\lib\*;target\conf example.Example
```
does not work (probably because Springboot does not launch the main class directly):
```java
Error: Could not find or load main class example.Example
```

### Dependencies

The dependencies are packeded in the final jar:

```sh
jar tvf target/example.logback.jar  | grep \\.jar | awk -F / '{print $NF}' |sort
```

shows the expected dependency jars.


### Testing Springboot App on developer machine
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
and check the appearance of new messages in `App.log` in the `logs` folder:
```sh
tail -3 logs/App.log
```
```sh
41629 [http-nio-8080-exec-1] INFO  example.Example - exampleHandler received: 20
41629 [http-nio-8080-exec-1] WARN  example.Example - exampleHandler received: 20
41629 [http-nio-8080-exec-1] DEBUG example.Example - exampleHandler received: 20
```

### Testing in Docker Container

```sh
mvn -Dmaven.test.skip=true clean package
IMAGE=basic-logback
docker container prune -f
docker image rm $IMAGE
```
```sh
sed -i "s|UID=.*|UID=$UID|g" Dockerfile
docker build -f Dockerfile -t $IMAGE .
test -d logs || mkdir logs
chmod 775 logs
NAME='basic-logback-container'
docker run --name $NAME -v $(pwd)/logs:/work/logs:rw -p 8080:8080 $IMAGE
```
to verify connect into the contaiter as specific user
```sh
docker exec -u $(id  -u) -it $NAME sh
```
```sh
whoami
```
```sh
myuser
```
then inspect the `logs` folder:
```sh
ls -l /work/logs
```
```sh
-rw-r--r--    1 myuser   myuser         586 Oct  7 13:17 App.2020-10-07.7.log.gz
-rw-r--r--    1 myuser   myuser         529 Oct  7 13:17 App.2020-10-07.8.log.gz
-rw-r--r--    1 myuser   myuser         436 Oct  7 13:17 App.2020-10-07.9.log.gz
-rw-r--r--    1 myuser   myuser        1079 Oct  7 13:24 App.log
```
```sh
tail /work/logs/App.log
```
Alternatively, inspect  the logs folder on the host:
```sh
ls -l logs
```
```sh
-rw-r--r-- 1 sergueik systemd-journal  586 Oct  7 15:17 App.2020-10-07.7.log.gz
-rw-r--r-- 1 sergueik systemd-journal  529 Oct  7 15:17 App.2020-10-07.8.log.gz
-rw-r--r-- 1 sergueik systemd-journal  436 Oct  7 15:17 App.2020-10-07.9.log.gz
-rw-r--r-- 1 sergueik systemd-journal 1079 Oct  7 15:24 App.log
```

```sh
tail logs/App.log
```
you will see the unique `data` values posted via `curl` loop parameter

Finally
```sh
docker container rm $NAME
```
#### Note:
if the `logs` directory is emptied while container is already started, the loggging appears to stop.

### Canary Testing

invalid `logback.xml` is easy to discover with this app:
```xml
  <fileNamePattern>>logs/${FILENAME}.%d{yyyy-MM-dd}.%i.log.gz</fileNamePattern>
```
```sh
java.lang.IllegalStateException: Failed to load ApplicationContext
Caused by: java.lang.IllegalStateException: Logback configuration error detected:
ERROR in ch.qos.logback.core.rolling.RollingFileAppender[FILE] - openFile(logs/App.log,true) call failed. java.io.FileNotFoundException: logs/App.log (Permission denied)
```

in some runs it leads to creating a direcory verbatim:

```sh
drwxrwxr-x  2 sergueik sergueik  4096 Sep 17 21:04  logs
drwxrwxr-x  2 sergueik sergueik  4096 Sep 17 20:57 '>logs'
```
however the attribute error

```xml
  <property name="DIR" value=" logs"/>
  <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <!-- can use ${APP_HOME} -->
    <file>${DIR}/${FILENAME}.log</file>
```
(leading or teailing whitespace) causes no harm as the whitespace is trimmed
from the actual directory name.

### See Also


 * https://www.codingame.com/playgrounds/4497/configuring-logback-with-spring-boot
 * https://stackoverflow.com/questions/2602415/rolling-logback-logs-on-filesize-and-time
 * https://www.baeldung.com/java-logging-rolling-file-appenders
 * [JSON logging](https://mathieularose.com/logback-json/)
 * [hints](https://stackoverflow.com/questions/40576959/logback-jsonlayout-printing-all-logs-on-the-same-line) on parsing JSON logs from the log via `jq`
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

