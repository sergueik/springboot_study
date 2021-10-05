### Info

This project contains [minimal demo code of logback example](http://logback.qos.ch/manual/appenders.html) converted to a regular springboot application logging initialization and operation

### Usage

* test application locally
```sh
mvn clean package
mkdir dummy
BASEDIR=$(pwd)/dummy
java -jar target/example.logback.jar
```
or
```sh
mvn -Dspring.profiles.active=test spring-boot:run
```
or even
```sh
mvn package
java -Dspring.profiles.active=development -Dlogback.debug=true -jar target/example.logback.jar
```
and in separate console
```sh
for CNT in $(seq 1 1 10) ; do wget --quiet -O /dev/null 127.0.0.1:8080/example ; done
tail logs/dummy/App.log
```
use `$(hostname -i)` instead of `localhost` when neededed
or (windows)
```cmd
mvn clean package
mkdir dummy
set BASEDIR=%CD%\dummy
java -jar target\example.logback.jar
curl -vk 127.0.0.1:8080/example
tail dummy\logs\App.log
```
to see the logging to take place
```sh
37266 [http-nio-8080-exec-2] DEBUG example.LogHelper - exampleHandler received: null
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
__NOTE__ Configuring the Tomcat access log directory through the `application.yaml` or `application.properties` does not currently work. In adition these configuration files suppress console logging, which is not a desired effect.

__NOTE__: cannot run with the 2.3.4:
```sh
org.springframework.context.ApplicationContextException: Unable to start web server;
Caused by: org.springframework.boot.web.server.WebServerException: Unable to start embedded Tomcat
Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'formContentFilter' defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration.class]
Caused by: java.lang.NoClassDefFoundError: com/fasterxml/jackson/databind/exc/InvalidDefinitionException
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
sixeand check the messages in `App.log` and console:
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
Alternatively, inspect the logs folder on the host:
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

### Total Size Test

 *  rerun the curl command with great number of repetitions:
```sh
for C in $(seq 1 1 5000) ; do curl -vk $(hostname -i):8080/example &>/dev/null ; done
```
then measure the logs
```sh
du -h logs/
```
```sh
640K    logs/
```
it appears that `totalSizeCap` setting in `logback.xml` is honored, but the numbers are not considered exactly:

| totalSizeCap | `log` dir size  |
|-------------------------|--------------|
| 10KB         | 172K        |
| 40KB         | 640K      |

Note: switching the __logback-clasic__ version to the `1.3.0-alpha5` leads to runtime errors.
the workaround [solution](https://tridion.stackexchange.com/questions/253/logback-xml-limit-the-size-of-files) does not allow specifying the date `%d{yyyy-MM-dd}`
to be part of the filename, only allows the counter `%i`.
### Note
Specifying `triggeringPolicy` element alongside with `rollingPolicy` "SizeAndTimeBasedRollingPolicy" leads to runtime Logback configuration error and no logs created in log directory at all


### Debugging Logback
* leave just one `appender` in `logback.xml`
*  run app with logback debuging flag
```sh
mvn -Dlogback.debug=true spring-boot:run
```

### Processing environments

the settings made in `application.yml` are not visilbe directly in `logback.xml` e.g.
in `application.yml`
```yaml
spring:
  profiles:
    active: development
---
spring:
  profiles: development
suffix: dev
```
and in `logback.xml`
```xml
  <springProfile name="development">
    <property name="SUFFIX" value="${suffix}"/>
  </springProfile>
  <property name="FILENAME" value="App-${SUFFIX}"/>

```
then the log names will not have expected suffix:
```sh
ls  logs/
App-SUFFIX_IS_UNDEFINED.2021-06-29.0.log.gz  App-SUFFIX_IS_UNDEFINED.log
```


the solution is to make definition of `SUFFIX` in `logback.xml` and only expose the choice of the `profile`:
```xml

  <springProfile name="development">
    <property name="SUFFIX" value="dev"/>
  </springProfile>
```

this time the spring log will show
```txt

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v1.5.4.RELEASE)

23:12:56,589 |-INFO in c.q.l.co.rolling.helper.RenameUtil - Renaming file [...\dummy\logs\App-dev.log] to [...\dummy\logs\App-dev.2021-06-29.0.log46967852620284.tmp]
23:12:56,597 |-INFO in ch.qos.logback.core.rolling.helper.Compressor - GZ compressing [...\dummy\logs\App-dev.2021-06-29.0.log46967852620284.tmp] as [...\dummy\logs\App-dev.2021-06-29.0.log.gz]
```
and the filenames in the `logs` directory will be like intended.
### TODO
the logback configuration turns out to be somewhat fragile. Small tweaks to `logback.xml` and `application.yaml` lead logging to stopped and/or properties in the log filename to be filed with `UNDEFINED`:
```sh

```
NOTE:
using "phantom include"
```xml/UND
<include resource="org/springframework/boot/logging/logback/base.xml">
```
instead of
```xml
<include resource="org/springframework/boot/logging/logback/defaults.xml">
```
or adding the
```xml
 <include resource="/org/springframework/boot/logging/logback/file-appender.xml"/>
```
in `logback.xml` may cause __logback__ to instantiate an additional `FILE` logger with its own default settings for file (values like `LOG_FILE_IS_UNDEFINED` and`/tmp/springboot.log`  are possible) and rotation policy (10Mb in size)
```sh
72c4 - URL [jar:file:/home/sergueik/src/springboot_study/basic-logback/target/example.logback.jar!/BOOT-INF/lib/spring-boot-1.5.4.RELEASE.jar!/org/springframework/boot/logging/logback/file-appender.xml] is not of type file
22:53:18,293 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - About to instantiate appender of type [ch.qos.logback.core.rolling.RollingFileAppender]
22:53:18,294 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - Naming appender as [FILE]
22:53:18,295 |-INFO in ch.qos.logback.core.joran.action.NestedComplexPropertyIA - Assuming default type [ch.qos.logback.classic.encoder.PatternLayoutEncoder] for [encoder] property
22:53:18,298 |-INFO in ch.qos.logback.core.rolling.FixedWindowRollingPolicy@16b3fc9e - No compression will be used
22:53:18,300 |-INFO in ch.qos.logback.core.rolling.RollingFileAppender[FILE] - Active log file name: LOG_FILE_IS_UNDEFINED
```
### See Also

  * https://www.codingame.com/playgrounds/4497/configuring-logback-with-spring-boot
  * https://stackoverflow.com/questions/2602415/rolling-logback-logs-on-filesize-and-time
  * https://www.baeldung.com/java-logging-rolling-file-appenders
  * [JSON logging](https://mathieularose.com/logback-json/)
  * [hints](https://stackoverflow.com/questions/40576959/logback-jsonlayout-printing-all-logs-on-the-same-line) on parsing JSON logs from the log via `jq`
  * https://www.programmersought.com/article/95552030197/
  * https://tridion.stackexchange.com/questions/253/logback-xml-limit-the-size-of-files
  * another combination of policies [example](https://stackify.com/logging-logback/) (non-working)
  * few more [examples](https://mkyong.com/logging/logback-xml-example/) including obsolete but stated to work one with `ch.qos.logback.core.rolling.SizeAndTimeBasedFNATP`

  * XML versus properties file - many places. also the consensus is the default file appender is size based (10MB) and cannot be overriden in application.properties alone
  * https://howtodoinjava.com/spring-boot2/logging/profile-specific-logging/

  * https://lankydan.dev/2019/01/09/configuring-logback-with-spring-boot
  * https://stackoverflow.com/questions/29918323/how-to-configure-rolling-file-appender-within-spring-boots-application-yml
  * https://mkyong.com/spring-boot/spring-boot-profile-based-properties-and-yaml-example/
  * https://www.baeldung.com/spring-boot-disable-console-logging

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
