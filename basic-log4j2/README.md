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
mvn -Dlog4j.debug spring-boot:run
```
this will print  various verbose log4j iscovery messages in startup log:


```text
DEBUG StatusLogger PluginManager 'Converter' found 48 plugins
DEBUG StatusLogger Starting OutputStreamManager SYSTEM_OUT.false.false-1
DEBUG StatusLogger Initializing Thread Context Data Service Providers
DEBUG StatusLogger Starting LoggerContext[name=7ebc9f63, org.apache.logging.log4j.core.LoggerContext@39ec99c]...
DEBUG StatusLogger Reconfiguration started for context[name=7ebc9f63] at URI null (org.apache.logging.log4j.core.LoggerContext@39ec99c) with optional ClassLoader: null
DEBUG StatusLogger Thread Context Data Service Provider initialization complete
DEBUG StatusLogger PluginManager 'ConfigurationFactory' found 5 plugins
DEBUG StatusLogger Missing dependencies for Yaml support, ConfigurationFactory rg.apache.logging.log4j.core.config.yaml.YamlConfigurationFactory is inactive
DEBUG StatusLogger Using configurationFactory org.apache.logging.log4j.core.config.ConfigurationFactory$Factory@1fc8308e
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.properties] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.properties] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yml] using context classloader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yaml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.yaml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.json] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.json] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.jsn] using context classloader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.jsn] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.xml] using context classloader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.xml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.springboot] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test7ebc9f63.springboot] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.properties] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.properties] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.yml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.yml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.yaml] using context class loaderjava.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.yaml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.json] using context class loaderjava.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.json] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.jsn] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.jsn] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.xml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.xml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2-test.springboot] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j2-test.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j2-test.springboot] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.properties] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.properties] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.properties] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.yml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.yaml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yaml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.yaml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.json] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.json] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.json] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.jsn] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.jsn] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.jsn] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.xml] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.xml] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.xml] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j27ebc9f63.springboot] using context class loader java.net.URLClassLoader@7ebc9f63.
TRACE StatusLogger Trying to find [log4j27ebc9f63.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.springboot] using java.net.URLClassLoader@7ebc9f63 class loader.
TRACE StatusLogger Trying to find [log4j27ebc9f63.springboot] using ClassLoader.getSystemResource().
TRACE StatusLogger Trying to find [log4j2.properties] using context class loader java.net.URLClassLoader@7ebc9f63.
DEBUG StatusLogger Apache Log4j Core 2.17.1 initializing configuration org.apache.logging.log4j.core.config.properties.PropertiesConfiguration@6115793
DEBUG StatusLogger Installed 1 script engine
DEBUG StatusLogger Oracle Nashorn version: 1.8.0_151, language: ECMAScript, threading: Not Thread Safe, compile: true, names: [nashorn, Nashorn, js, JS, JavaScript, javascript, ECMAScript, ecmascript], factory class: jdk.nashorn.api.scripting.NashornScriptEngineFactory
DEBUG StatusLogger PluginManager 'Core' found 128 plugins
DEBUG StatusLogger PluginManager 'Level' found 0 plugins
DEBUG StatusLogger PluginManager 'Lookup' found 17 plugins
DEBUG StatusLogger Building Plugin[name=loggers, class=org.apache.logging.log4j.core.config.LoggersPlugin].
DEBUG StatusLogger createLoggers(={})
DEBUG StatusLogger Building Plugin[name=appenders, class=org.apache.logging.log4j.core.config.AppendersPlugin].
DEBUG StatusLogger createAppenders(={})
WARN StatusLogger No Root logger was configured, creating default ERROR-level Root logger with Console appender
DEBUG StatusLogger PluginManager 'Converter' found 48 plugins
DEBUG StatusLogger Starting OutputStreamManager SYSTEM_OUT.false.false-2
DEBUG StatusLogger Configuration org.apache.logging.log4j.core.config.properties.PropertiesConfiguration@6115793 initialized
DEBUG StatusLogger Starting configuration org.apache.logging.log4j.core.config.properties.PropertiesConfiguration@6115793
DEBUG StatusLogger Started configuration org.apache.logging.log4j.core.config.properties.PropertiesConfiguration@6115793 OK.
TRACE StatusLogger Stopping org.apache.logging.log4j.core.config.DefaultConfiguration@6ad4eda0...
TRACE StatusLogger DefaultConfiguration notified 1 ReliabilityStrategies that config will be stopped.
TRACE StatusLogger DefaultConfiguration stopping root LoggerConfig.
TRACE StatusLogger DefaultConfiguration notifying ReliabilityStrategies that appenders will be stopped.
TRACE StatusLogger DefaultConfiguration stopping remaining Appenders.
DEBUG StatusLogger Shutting down OutputStreamManager SYSTEM_OUT.false.false-1DEBUG StatusLogger OutputStream closed
DEBUG StatusLogger Shut down OutputStreamManager SYSTEM_OUT.false.false-1, all resources released: true
DEBUG StatusLogger Appender DefaultConsole-1 stopped with status true
TRACE StatusLogger DefaultConfiguration stopped 1 remaining Appenders.
TRACE StatusLogger DefaultConfiguration cleaning Appenders from 1 LoggerConfigs.

DEBUG StatusLogger Stopped org.apache.logging.log4j.core.config.DefaultConfiguration@6ad4eda0 OK
TRACE StatusLogger Reregistering MBeans after reconfigure. Selector=org.apache.logging.log4j.core.selector.ClassLoaderContextSelector@4fbd8127
TRACE StatusLogger Reregistering context (1/1): '7ebc9f63' org.apache.logging.log4j.core.LoggerContext@39ec99c
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=StatusLogger'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=ContextSelector'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=Loggers,name=*'TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=Appenders,name=*'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=AsyncAppenders,name=*'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=AsyncLoggerRingBuffer'
TRACE StatusLogger Unregistering but no MBeans found matching 'org.apache.logging.log4j2:type=7ebc9f63,component=Loggers,name=*,subtype=RingBuffer'
DEBUG StatusLogger Registering MBean org.apache.logging.log4j2:type=7ebc9f63
DEBUG StatusLogger Registering MBean org.apache.logging.log4j2:type=7ebc9f63,component=StatusLogger

```
and  produce the messages in `App.log` and console:
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


