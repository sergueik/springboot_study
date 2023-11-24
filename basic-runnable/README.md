### Info

This directory contains basic example from
[repo](https://github.com/eugenp/tutorials/blob/master/core-java-modules/core-java-concurrency-basic/src/main/java/com/baeldung/concurrent) of the
__Implementing a Runnable vs Extending a Thread__ [article](https://www.baeldung.com/java-runnable-vs-extending-thread)

### Usage


below , describes the fixed version. For early failed attempts, see commit history

```sh
mvn spring-boot:run
```
```text
2023-11-24 14:32:18.841  INFO 11432 --- [           main] example.Application                   : No active profile set, falling back to default profiles: default
2023-11-24 14:32:19.717  INFO 11432 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8080 (http)
2023-11-24 14:32:19.726  INFO 11432 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2023-11-24 14:32:19.727  INFO 11432 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
2023-11-24 14:32:19.782  INFO 11432 --- [           main] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2023-11-24 14:32:19.783  INFO 11432 --- [           main] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 889 ms
2023-11-24 14:32:19.931  INFO 11432 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2023-11-24 14:32:20.060  INFO 11432 --- [           main] o.s.s.c.ThreadPoolTaskScheduler          : Initializing ExecutorService 'taskScheduler'
2023-11-24 14:32:20.093  INFO 11432 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''

2023-11-24 14:32:20.104  INFO 11432 --- [           main] example.Application                      : Started Application in 1.564 seconds (JVM running for 1.876)
2023-11-24 14:32:20.120  INFO 11432 --- [cTaskExecutor-1] example.task.EventLoggingTask            : os.arch = amd64
2023-11-24 14:32:20.121  INFO 11432 --- [cTaskExecutor-1] example.task.EventLoggingTask            : Run with value1 = 42, profile = development, applicationPath = c:\program files (x86), expandEnvVar = C:\Users\Serguei through annotation
2023-11-24 14:32:20.122  INFO 11432 --- [cTaskExecutor-1] example.task.EventLoggingTask            : Read confguration from resource "/application.properties" within application: value1 = 42
```
### See Also:

   * https://stackoverflow.com/questions/15818839/value-annotation-doesnt-return-a-value
   * https://stackoverflow.com/questions/5853167/runnable-with-a-parameter
   * https://www.baeldung.com/java-thread-parameters

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
