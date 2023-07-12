### Info

This directory contains log4j2 example demo project logging to console only

### Usage


#### Run Locally

```sh
mvn clean package
```
```sh
java  -cp target/example.log4j2.jar:target/lib/* example.Example
```
  * observe messages in console:
```text
DEMO 2023-07-12 18:34:39  [main] - INFO  Info log message
DEMO 2023-07-12 18:34:39  [main] - ERROR Error log message
DEMO 2023-07-12 18:34:39  [main] - WARN  Warn log message
DEMO 2023-07-12 18:34:39  [main] - FATAL Fatal log message
```
#### test in Docker Container
  * save configuration
```sh
cp src/main/resources/log4j2.properties  src/main/resources/log4j2.properties.SAVED
```
  * modify the logger configuration after the application was packaged. E.g. modify the pattern expression:
```java
appender.console.layout.pattern = NEW DEMO %d{yyyy-MM-dd HH:mm:ss} %X{trace.id} [%t] - %-5p %msg%n
```
  * with SpringBoot application build and run basic alpine image and observre that the modifications in `log4j.properties` appear to be ignored - the log4j seems to use some kind of default pattern layout.
```sh
IMAGE=basic-log4j2
docker build -t $IMAGE -f Dockerfile .
```
  * run:
```sh
docker run --rm -it $IMAGE
```
  * if need to troubleshoot
```sh
docker run --entrypoint "" -it $IMAGE sh
```
Add `log4j2.xml` with `Console` logger (WIP):
```XML
```
* run with log4j2 debug option
```sh
java -Dlog4j2.debug -cp target/example.log4j2.jar:target/lib/* example.Example
```
* observe the choice of logger and other information (truncated):
```text
DEBUG StatusLogger Using ShutdownCallbackRegistry class org.apache.logging.log4j.core.util.DefaultShutdownCallbackRegistry
...
DEBUG StatusLogger Apache Log4j Core 2.17.1 initializing configuration org.apache.logging.log4j.core.config.properties.PropertiesConfiguration@3234e239
DEBUG StatusLogger Installed 1 script engine
DEBUG StatusLogger Oracle Nashorn version: 1.8.0_161, language: ECMAScript, threading: Not Thread Safe, compile: true, names: [nashorn, Nashorn, js, JS, JavaScript, javascript, ECMAScript, ecmascript], factory class: jdk.nashorn.api.scripting.NashornScriptEngineFactory
DEBUG StatusLogger PluginManager 'Core' found 128 plugins
DEBUG StatusLogger PluginManager 'Level' found 0 plugins
DEBUG StatusLogger PluginManager 'Lookup' found 17 plugins
DEBUG StatusLogger Building Plugin[name=AppenderRef, class=org.apache.logging.log4j.core.config.AppenderRef].
TRACE StatusLogger TypeConverterRegistry initializing.
DEBUG StatusLogger PluginManager 'TypeConverter' found 26 plugins
DEBUG StatusLogger createAppenderRef(ref="System.stdout", level="null", Filter=null)
DEBUG StatusLogger Building Plugin[name=root, class=org.apache.logging.log4j.core.config.LoggerConfig$RootLogger].
DEBUG StatusLogger createLogger(additivity="null", level="INFO", includeLocation="null", ={System.stdout}, ={}, Configuration, Filter=null)
DEBUG StatusLogger Building Plugin[name=loggers, class=org.apache.logging.log4j.core.config.LoggersPlugin].
DEBUG StatusLogger createLoggers(={root})
DEBUG StatusLogger Building Plugin[name=layout, class=org.apache.logging.log4j.core.layout.PatternLayout].
DEBUG StatusLogger PatternLayout$Builder(pattern="DEMO %d{yyyy-MM-dd HH:mm:ss} %X{trace.id} [%t] - %-5p %msg%n", PatternSelector=null, Configuration, Replace=null, charset="null", alwaysWriteExceptions="null", disableAnsi="null", noConsoleNoAnsi="null", header="null", footer="null")
...
DEBUG StatusLogger LoggerContext[name=33909752, org.apache.logging.log4j.core.LoggerContext@131276c2] started OK.
DEMO 2023-07-12 18:37:38  [main] - INFO  Info log message
DEMO 2023-07-12 18:37:38  [main] - ERROR Error log message
DEMO 2023-07-12 18:37:38  [main] - WARN  Warn log message
DEMO 2023-07-12 18:37:38  [main] - FATAL Fatal log message
DEBUG StatusLogger Stopping LoggerContext[name=33909752, org.apache.logging.log4j.core.LoggerContext@131276c2]
```
* when application is a Spring Boot one, it seems to use SLF4J and completely ignore provided `Pattern`:

```text
DEBUG StatusLogger Using ShutdownCallbackRegistry class org.apache.logging.log4j.core.util.DefaultShutdownCallbackRegistry
DEBUG StatusLogger org.slf4j.helpers.Log4jLoggerFactory is not on classpath. Good!
WARN StatusLogger Multiple logging implementations found:
Factory: org.apache.logging.log4j.core.impl.Log4jContextFactory, Weighting: 10
Factory: org.apache.logging.slf4j.SLF4JLoggerContextFactory, Weighting: 15
Using factory: org.apache.logging.slf4j.SLF4JLoggerContextFactory
```
  * interrupt the running container
### Test XML Configuration
* rename the `log4j2.properties` and `log4j2.xml` to let the latter be used
* run in debug mode to see the syntax error message
#### Cleanup

```sh
docker container ls -a | grep "${IMAGE}" | awk '{print $1}' | xargs -IX docker container rm X

docker image prune -f
```
### See Also
    * [log4j2 – Logging to Both File and Console](https://www.baeldung.com/java-log4j2-file-and-console) and [example repository](https://github.com/eugenp/tutorials/tree/master/logging-modules/log4j2)
 
    * [log4j Pattern Layout](http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html) - note the format is similar for lof4j2 but the good documentation link is yet to be found.
   * [example](https://www.toolbox.com/tech/programming/question/how-to-implement-log4j-in-java-application-050809/) adding `log4j2` without `slf4j`
   * https://stackoverflow.com/questions/19574413/log4j-2-how-get-log4js-debug-messages

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


