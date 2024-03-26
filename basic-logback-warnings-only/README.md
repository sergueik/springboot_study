### Info

Logging of WARN only to filebased appender while everything to CONSOLE

based on [stackoverflow discussion example](https://stackoverflow.com/questions/25339540/logback-configuration-file-write-all-except-debug-to-file-as-logging-to-cons) 

### Usage

* test application locally

#### Console-only Logging And WARNING to File

```sh
mvn clean spring-boot:run
```
this will call loggers in the servler contstructor:
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
for CNT in $(seq 1 1 10) ; do wget --quiet -O /dev/null 127.0.0.1:8080/example ; done
```
will see the log messages in the application console
```text
50609 [http-nio-8080-exec-1] INFO  o.a.c.c.C.[Tomcat].[localhost].[/] - Initializing Spring DispatcherServlet 'dispatcherServlet'
50609 [http-nio-8080-exec-1] INFO  o.s.web.servlet.DispatcherServlet - Initializing Servlet 'dispatcherServlet'
50613 [http-nio-8080-exec-1] INFO  o.s.web.servlet.DispatcherServlet - Completed initialization in 4 ms
50633 [http-nio-8080-exec-1] INFO  example.LogHelper - INFO: request processed: null
50634 [http-nio-8080-exec-1] WARN  example.LogHelper - WARN: request processed: null
50661 [http-nio-8080-exec-2] INFO  example.LogHelper - INFO: request processed: null
50661 [http-nio-8080-exec-2] WARN  example.LogHelper - WARN: request processed: null
50667 [http-nio-8080-exec-3] INFO  example.LogHelper - INFO: request processed: null
50667 [http-nio-8080-exec-3] WARN  example.LogHelper - WARN: request processed: null
50674 [http-nio-8080-exec-5] INFO  example.LogHelper - INFO: request processed: null
50674 [http-nio-8080-exec-5] WARN  example.LogHelper - WARN: request processed: null
50680 [http-nio-8080-exec-6] INFO  example.LogHelper - INFO: request processed: null
50680 [http-nio-8080-exec-6] WARN  example.LogHelper - WARN: request processed: null
50687 [http-nio-8080-exec-7] INFO  example.LogHelper - INFO: request processed: null
50687 [http-nio-8080-exec-7] WARN  example.LogHelper - WARN: request processed: null
50692 [http-nio-8080-exec-8] INFO  example.LogHelper - INFO: request processed: null
50692 [http-nio-8080-exec-8] WARN  example.LogHelper - WARN: request processed: null
50697 [http-nio-8080-exec-9] INFO  example.LogHelper - INFO: request processed: null
50698 [http-nio-8080-exec-9] WARN  example.LogHelper - WARN: request processed: null
50703 [http-nio-8080-exec-10] INFO  example.LogHelper - INFO: request processed: null
50703 [http-nio-8080-exec-10] WARN  example.LogHelper - WARN: request processed: null
50707 [http-nio-8080-exec-1] INFO  example.LogHelper - INFO: request processed: null
```
and in file `a.log`:
```text
16:40:30.267 [http-nio-8080-exec-1] WARN  example.LogHelper - WARN: request processed: null
16:40:30.294 [http-nio-8080-exec-2] WARN  example.LogHelper - WARN: request processed: null
16:40:30.300 [http-nio-8080-exec-3] WARN  example.LogHelper - WARN: request processed: null
16:40:30.307 [http-nio-8080-exec-5] WARN  example.LogHelper - WARN: request processed: null
16:40:30.313 [http-nio-8080-exec-6] WARN  example.LogHelper - WARN: request processed: null
16:40:30.320 [http-nio-8080-exec-7] WARN  example.LogHelper - WARN: request processed: null
16:40:30.325 [http-nio-8080-exec-8] WARN  example.LogHelper - WARN: request processed: null
16:40:30.331 [http-nio-8080-exec-9] WARN  example.LogHelper - WARN: request processed: null
16:40:30.336 [http-nio-8080-exec-10] WARN  example.LogHelper - WARN: request processed: null
16:40:30.340 [http-nio-8080-exec-1] WARN  example.LogHelper - WARN: request processed: null
```

### See Also:
   * https://logback.qos.ch/manual/filters.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)




