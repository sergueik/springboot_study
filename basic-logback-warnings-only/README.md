### Info

Logging of `WARN` only to `FILE`-based appender while `INFO` and everything more severe to `CONSOLE`

based on [stackoverflow discussion example](https://stackoverflow.com/questions/25339540/logback-configuration-file-write-all-except-debug-to-file-as-logging-to-cons) 

### NOTE

the level threshold is set to very lowest level at the global scope:
```XML
<root level="DEBUG">
</root>
```
but is reset to `INFO` and `WARN` levels in `CONSOLE` and `FILE` Appenders through direct usage of `Filter`:
```XML
<filter class="ch.qos.logback.classic.filter.ThresholdFilter">
      <level>WARN</level>
    </filter>
```
this is to illystrate suppression of `DEBUG` messages in console and `DEBUG` and `INFO` in the log file `app.log`

### Usage

* test application locally

#### Console-only Logging And WARNING to File
* Windows
```cmd
copy NUL app.log
```
```sh

mvn clean spring-boot:run
```
this will call loggers in the servlet logger helper class contstructor:
```java
logger = LoggerFactory.getLogger(LogHelper.class);
logger.info("logger constructor INFO message");
logger.warn("logger constructor WARN message");
logger.debug("logger constructor DEBUG message");
```
and endpoint controller
```java
@GetMapping
public String exampleHandler(@RequestParam(required = false) String data) {
	logger.info("INFO: {}", data);
	logger.warn("WARN: {}", data);
	logger.debug("DEBUG: {}", data);
	return ("request processed: " + data);
}

```
will see console logs:
```text
98  [main] INFO  example.Example - Starting Example on lenovoy40-1 with PID 14493 (/home/sergueik/src/springboot_study/basic-logback-warnings-only/target/classes started by sergueik in /home/sergueik/src/springboot_study/basic-logback-warnings-only)
500  [main] INFO  example.Example - No active profile set, falling back to default profiles: default
1239 [main] INFO  o.s.b.w.e.tomcat.TomcatWebServer - Tomcat initialized with port(s): 8080 (http)
1245 [main] INFO  o.a.coyote.http11.Http11NioProtocol - Initializing ProtocolHandler ["http-nio-8080"]
1245 [main] INFO  o.a.catalina.core.StandardService - Starting service [Tomcat]
1246 [main] INFO  o.a.catalina.core.StandardEngine - Starting Servlet engine: [Apache Tomcat/9.0.38]
1366 [main] INFO  o.a.c.c.C.[Tomcat].[localhost].[/] - Initializing Spring embedded WebApplicationContext
1366 [main] INFO  o.s.b.w.s.c.ServletWebServerApplicationContext - Root WebApplicationContext: initialization completed in 822 ms
1398 [main] INFO  example.LogHelper - logger constructor INFO message
1398 [main] WARN  example.LogHelper - logger constructor WARN message
1514 [main] INFO  o.s.s.c.ThreadPoolTaskExecutor - Initializing ExecutorService 'applicationTaskExecutor'
1604 [main] INFO  o.a.coyote.http11.Http11NioProtocol - Starting ProtocolHandler ["http-nio-8080"]
1618 [main] INFO  o.s.b.w.e.tomcat.TomcatWebServer - Tomcat started on port(s): 8080 (http) with context path ''
1641 [main] INFO  example.Example - Started Example in 1.375 seconds (JVM running for 1.817)

```
see in the file `app.log` :
```text
16:39:41.031 [main] WARN  example.LogHelper - logger constructor WARN message
```

alternatively
```sh
mvn package
java -jar target/example.logback-warnings-only.jar
```

and interact with app through separate shell
```sh
for CNT in $(seq 1 1 3) ; do wget --quiet -O /dev/null 127.0.0.1:8080/example ; done
```
NOTE: on Windows / Git bash there will be no `wget` and run the following command instead:
```sh
 for CNT in $(seq 1 1 3) ; do curl -s 127.0.0.1:8080/example ; done
```
this will print in the client console
```text
request processed: null
request processed: null
request processed: null
```
and one will observe the new log messages in the application console
```text
96518 [http-nio-8080-exec-1] INFO  example.LogHelper - INFO: request processed:null
96526 [http-nio-8080-exec-1] WARN  example.LogHelper - WARN: request processed:null
96694 [http-nio-8080-exec-3] INFO  example.LogHelper - INFO: request processed:null
96694 [http-nio-8080-exec-3] WARN  example.LogHelper - WARN: request processed:null
96814 [http-nio-8080-exec-5] INFO  example.LogHelper - INFO: request processed:null
96814 [http-nio-8080-exec-5] WARN  example.LogHelper - WARN: request processed:null
```
and in the file `app.log`:
```text
18:43:32.223 [http-nio-8080-exec-1] WARN  example.LogHelper - WARN: request processed: null
18:43:32.391 [http-nio-8080-exec-3] WARN  example.LogHelper - WARN: request processed: null
18:43:32.511 [http-nio-8080-exec-5] WARN  example.LogHelper - WARN: request processed: null
```

### Note 
if the `janino` dependency is not added to project `pom.xml` the following exception will be observed in runtime:
```text
Exception in thread "main" java.lang.NoSuchMethodError: ch.qos.logback.core.util.OptionHelper.isNotEmtpy([Ljava/lang/Object;)Z
```
### See Also:
   * https://logback.qos.ch/manual/filters.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
