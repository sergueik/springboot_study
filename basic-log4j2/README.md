### Info

This directory contains log4j2 example demo project

### Usage

#### Dependency project (optional)

To have the rabbitmq logger turned on sync to commit `655a47663f84ee842a6f3b61cb98a3ce39cf606a`. Otherwise you need not build that logger explicitly - it was dissabled
```sh
pushd ../basic-rabbitmq-appender
mvn package
cp target/example.rabbitmq-appender-0.1.0-SNAPSHOT.jar ../basic-log4j2/src/main/resources/
popd
```
#### Testing Springboot App locally


```sh
mvn spring-boot:run
```
and check the messages in `App.log` and console:
```sh
21:35:06.166 [main] INFO o.s.b.c.e.t.TomcatEmbeddedServletContainer - Tomcat started on port(s): 8085 (http)
```
and
```sh
[main] INFO 22ogger - init message
[main] INFO 22ogger - init message
[main] WARN 23ogger - init message
```
in `App.log` only
then
```sh
for cnt in $(seq 0 1 3); do curl "http://localhost:8085/example?data='${cnt}+test'"; done
```
and check the appearance of new messages in `App.log` and console:
```sh
[http-nio-8085-exec-10] INFO 20ogger - raw data '0 test'
[http-nio-8085-exec-10] INFO 20ogger - raw data '0 test'
[http-nio-8085-exec-10] INFO 20ogger - handler received: '0 test'
[http-nio-8085-exec-10] INFO 20ogger - handler received: '0 test'
[http-nio-8085-exec-9] INFO 20ogger - raw data '1 test'
[http-nio-8085-exec-9] INFO 20ogger - raw data '1 test'
[http-nio-8085-exec-9] INFO 20ogger - handler received: '1 test'
[http-nio-8085-exec-9] INFO 20ogger - handler received: '1 test'
[http-nio-8085-exec-8] INFO 20ogger - raw data '2 test'
[http-nio-8085-exec-8] INFO 20ogger - raw data '2 test'
[http-nio-8085-exec-8] INFO 20ogger - handler received: '2 test'
[http-nio-8085-exec-8] INFO 20ogger - handler received: '2 test'
[http-nio-8085-exec-7] INFO 20ogger - raw data '3 test'
[http-nio-8085-exec-7] INFO 20ogger - raw data '3 test'
[http-nio-8085-exec-7] INFO 20ogger - handler received: '3 test'
[http-nio-8085-exec-7] INFO 20ogger - handler received: '3 test'
```
### Alternative log4j2 Configurations

There are two property file (combining those is still work in progress):

```sh
log4j2.properties.CONSOLE-ONLY
log4j2.properties.FILE-ONLY
```
remove the and rename one of the above to simply `log4j2.properties` to see it work


### Testing in Docker Container
#### Basic permission check


```sh
IMAGE='centos:7'
(docker container run --rm -v $(pwd)/logs:/work/logs:rw -u $(id -u ${USER}):$(id -g ${USER}) $IMAGE touch /work/logs/dummy_file ) ; ls -l logs/dummy_file
```
```sh
-rw-r--r-- 1 sergueik sergueik 0 Jan 22 18:20 logs/dummy_file
```
NOTE: bare bones alpine images have `ENTRYPOINT`:

```sh
docker image inspect $IMAGE | jq '.[]|.Config|.Entrypoint'
```
```sh
[
  "/bin/sh",
  "-c",
  "[\"sh\", \"/wait_for.sh\", \"--host=${SERVICE_HOST}\" , \"--port=${SERVICE_\tPORT}\", \"--timeout=${TIMEOUT}\"]"
]

```
this presumably is leading to the following error in the elementary command
```sh
IMAGE=alpine:3.9
docker container run --rm -v $(pwd)/logs:/work/logs:rw -u $(id -u ${USER}):$(id -g ${USER}) $IMAGE touch /work/logs/dummy_file
```
```sh
touch: line 1: syntax error: bad substitutio
```
which shows even in
		
```sh
docker container run -it alpine:3.9
```
- compare with `centos:7` images where the same is blank
#### Application Check

* modify the `log4j2.properties` to include the `logs` folder:
```java

# adjust log file path as per your need
property.filename = logs/App.log
appender.rolling.filePattern = logs/App-%d{MM-dd-yy-HH-mm-ss}-%i.log.gz
```
* repackage the jar
```sh
mvn clean package
```
* build basic alpine image
```sh
IMAGE=basic-log4j2
docker build -t $IMAGE -f Dockerfile .
```
* run without volume map:
```sh
```

* repeat curl command 
* list logs in container
```sh
docker exec -w /work -it $(docker container ls | grep $IMAGE | awk '{print $1}' ) sh -c 'ls -l logs'
```
```text
total 24
-rw-r--r--    1 myuser   myuser          98 Mar 15 15:30 App-03-15-22-15-20-59-1.log.gz
-rw-r--r--    1 myuser   myuser         118 Mar 15 15:30 App-03-15-22-15-30-06-1.log.gz
-rw-r--r--    1 myuser   myuser         101 Mar 15 15:30 App-03-15-22-15-30-07-1.log.gz
-rw-r--r--    1 myuser   myuser         126 Mar 15 15:30 App-03-15-22-15-30-10-1.log.gz
-rw-r--r--    1 myuser   myuser         126 Mar 15 15:30 App-03-15-22-15-30-11-1.log.gz
-rw-r--r--    1 myuser   myuser         508 Mar 15 15:30 App.log

```
*  stop container
```sh
CONTAINER=$(docker container ls | grep $IMAGE | awk '{print $1}' )
docker stop $CONTAINER
docker rm $CONTAINER
```
* run with volume mapped
```sh
rm logs/*
docker run -d -p 8085:8085 -v $(pwd)/logs:/work/logs:rw $IMAGE
```
* check container console logs for errors
```sh
CONTAINER=$(docker container ls | grep $IMAGE | awk '{print $1}' )
docker logs $CONTAINER |less
```
```text

2022-03-15 15:34:22,854 main ERROR Unable to create file logs/App.log java.io.IOException: Permission denied
        at java.io.UnixFileSystem.createFileExclusively(Native Method)
        at java.io.File.createNewFile(File.java:1012)
        at org.apache.logging.log4j.core.appender.rolling.RollingFileManager$RollingFileManagerFactory.createManager(RollingFileManager.java:733)
        at org.apache.logging.log4j.core.appender.rolling.RollingFileManager$RollingFileManagerFactory.createManager(RollingFileManager.java:716)
        at org.apache.logging.log4j.core.appender.AbstractManager.getManager(AbstractManager.java:114)
        at org.apache.logging.log4j.core.appender.OutputStreamManager.getManager(OutputStreamManager.java:100)
        at org.apache.logging.log4j.core.appender.rolling.RollingFileManager.getFileManager(RollingFileManager.java:217)
        at org.apache.logging.log4j.core.appender.RollingFileAppender$Builder.build(RollingFileAppender.java:146)
        at org.apache.logging.log4j.core.appender.RollingFileAppender$Builder.build(RollingFileAppender.java:62)
        at org.apache.logging.log4j.core.config.plugins.util.PluginBuilder.build(PluginBuilder.java:122)

```
* update `logs` permission to `777`:
```sh
sudo chmod 777 logs
```
* recycle continer, run again, confirm no erros in console logs
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
-rw-rw-r--    1 root     root      20547868 Mar 15 15:18 app.jar
-rw-rw-r--    1 root     root          1801 Mar 15 15:17 log4j2.properties
drwxrwxrwx    2 root     root          4096 Mar 15 15:39 logs

```
Note:
```sh
docker container ls | grep $IMAGE |  awk '{print $1}' | xargs -IX  docker exec -w '/work' -it X sh
```
fails with
```sh
the input device is not a TTY
```
one needs to use two lines:
```sh
ID=$(docker container ls | grep $IMAGE |  awk '{print $1}')
docker exec -it $ID sh
```
repeat curl command

```sh
for cnt in $(seq 0 1 3); do curl "http://localhost:8085/example?data='${cnt}+test'"; done
```


### DMC
* download a prebuilt Virtual Box ELK image e.g. from [bitnami](https://bitnami.com/stack/elk/virtual-machine)
and launch following their [instructions](https://docs.bitnami.com/virtual-machine/apps/elk/get-started/get-started/)

* download specific ELK APM agent jar version from `https://search.maven.org/artifact/co.elastic.apm/elastic-apm-agent/1.24.0/jar` interactively.

* build application jar locally
```sh
mvn clean package
```
* launch the bitnami ELK image. NOTE: the only stable networking choice appears to be NAT.
* copy application jar and elastic agent jar into the VM:
```sh
scp -P 2222 target/example.log4j2.jar  bitnami@localhost:
scp  -P 2222 elastic-apm-agent-1.24.0.jar  bitnami@localhost:
```
Log on to the VM
```sh
ssh -p 2222 bitnami@localhost
```
* run the jar locally
```sh
sudo -s
java -javaagent:elastic-apm-agent-1.24.0.jar -Delastic.apm.application_packages=example -Delastic.apm.server_urls=http://127.0.0.1:9200 -Delastic.apm.enable_log_correlation=true -Ddisable_send=true -jar example.log4j2.jar
```
* ssh to a separate session on the VM, access the appliction web interface locally from the VM
```sh
ssh -p 2222 bitnami@localhost
sudo -s
root@debian:/home/bitnami# curl "http://localhost:8085/example?data=1234"
```
in the console you will see
```
2021-07-13 21:15:18 trace.id=7e5cfd8005ffbb28347940aecb376019 INFO  LogHelper:24 - raw data 1234
2021-07-13 21:15:18 trace.id=7e5cfd8005ffbb28347940aecb376019 INFO  LogHelper:24 - handler received: 1234
```
These are two log lines from two different API in the toy application. The `trace.id` value is "sticky".

Note, when there is no real APM server running on port 9200, APM re-attempts to connect but reports the following failure (formatted for better readability):
```sh
2021-07-13 03:47:53.424 [apm-reporter] INFO co.elastic.apm.report.IntakeV2ReportingEventHandler - Backing off for 0 seconds (±10%)
2021-07-13 03:47:53.424 [apm-reporter] WARN co.elastic.apm.report.IntakeV2ReportingEventHandler - Server returned HTTP response code: 400 for URL: http://127.0.0.1:9200/intake/v2/events
2021-07-13 03:47:53.434 [apm-reporter] WARN co.elastic.apm.report.IntakeV2ReportingEventHandler -
{
  "error": {
    "root_cause": [
      {
        "type": "mapper_parsing_exception",
        "reason": "failed to parse"
      }
    ],
    "type": "mapper_parsing_exception",
    "reason": "failed to parse",
    "caused_by": {
      "type": "illegal_argument_exception",
      "reason": "Malformed content, found extra data after parsing: START_OBJECT"
    }
  },
  "status": 400
}
```
this is cured by installing the real APM server
### See Also

 * [JSON logging](https://www.baeldung.com/java-log-json-output)
 * [JSON layout](https://stackoverflow.com/questions/39590365/print-stacktrace-with-log4j2-in-json-with-jsonlayout-in-a-single-line)
 * Logback MDC [manual](http://logback.qos.ch/manual/mdc.html)
 * [ELK APM MDC configuration bug](https://github.com/elastic/apm-agent-java/issues/499) - contains valuable information covering how things should be set
 * [log4j Pattern Layout](http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html) - note the format is similar for lof4j2 but the good documentation link is yet to be found.
 * some [intro](https://blog.frankel.ch/logging-additional-metadata/) to MDC in ELK context
 * https://slacker.ro/2020/09/02/monitoring-java-applications-with-elastic-getting-started-with-the-elastic-apm-java-agent/
 * https://levelup.gitconnected.com/how-to-integrate-elastic-apm-java-agent-with-spring-boot-7ce8388a206e
 * https://github.com/gapperdan/hello-springboot-mdc
 * https://github.com/TiantianUpup/springboot-log/tree/master/springboot-trace (somewhat more complex than one would expect?)
 * https://github.com/VivyTeam/mdc-logger
 * https://github.com/sonamsamdupkhangsar/mdc-webapp-example
 * [example](https://www.toolbox.com/tech/programming/question/how-to-implement-log4j-in-java-application-050809/) adding `log4j2` without `slf4j`


### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


