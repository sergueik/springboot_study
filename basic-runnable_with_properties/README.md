### Info

This directory contains basic example from
[repo](https://github.com/eugenp/tutorials/blob/master/core-java-modules/core-java-concurrency-basic/src/main/java/com/baeldung/concurrent) of the
__Implementing a Runnable vs Extending a Thread__ [article](https://www.baeldung.com/java-runnable-vs-extending-thread) and demonstrating
defining the Runnable class properties from `application.properties` configuration for Task running on `org.springframework.core.task.SimpleAsyncTaskExecutor.SimpleAsyncTaskExecutor()` and `java.util.concurrent.Executors.newSingleThreadExecutor()`. In the properties file, defining os-specific values is shown

### Usage


* below, describes the fixed version. For early failed attempts, see commit historiaes

```sh
mvn spring-boot:run
```
```text
2023-11-25 09:49:22.423  INFO 10956 --- [           main] example.utils.Utils                   : in Utils: raw os name: windows 8.1
2023-11-25 09:49:22.424  INFO 10956 --- [           main] example.config.Config                 : Config: os name: windows x64
2023-11-25 09:49:22.439  INFO 10956 --- [cTaskExecutor-1] example.config.Config                 : Config: applicationPaths keys: [[windows x64, unix, windows]]
2023-11-25 09:49:22.439  INFO 10956 --- [cTaskExecutor-1] example.config.Config                 : Config:expandEnvVars keys: [[windows x64, unix, windows]]
2023-11-25 09:49:22.440  INFO 10956 --- [cTaskExecutor-1] example.task.EventLoggingTask         : Run with value = 42, profile = development, applicationPath = c:\program files (x86), expandEnvVar = C:\Users\Serguei through annotation
```

### Replacing Properties

* in the minimal configuration overriding the `spring.config.location` does not appear to work:
```sh
mvn -Dspring.config.location=alternative\application.properties clean spring-boot:run
```
set in `alternative\application.properties`
```java
setting.value = 101
```

still find from the console log that the `setting.value` setting being read from the embedded `src\main\resources\application.properties`:
```text
2023-11-28 08:31:30.791  INFO 1200 --- [cTaskExecutor-1] example.task.EventLoggingTask            : Run with value = 42, profile = development, applicationPath= c:\program files (x86), expandEnvVar = C:\Users\Serguei, applicationOsSpecificPath = c:\program files (x86) through annotation
2023-11-28 08:31:30.791  INFO 1200 --- [pool-2-thread-1] example.task.EventLoggingTask            : Run with value = 42, profile = development, applicationPath= c:\program files (x86), expandEnvVar = C:\Users\Serguei, applicationOsSpecificPath = c:\program files (x86) through annotation
```
comment the value in the `src\main\resources\application.properties`, then observe the `alternative\application.properties` being used:
```text
2023-11-28 08:37:27.354  INFO 9888 --- [pool-2-thread-1] example.task.EventLoggingTask            : Run with value = 101, profile = development, applicationPath = c:\program files (x86), expandEnvVar = C:\Users\Serguei, applicationOsSpecificPath = c:\program files (x86) through annotation
```
### See Also:

   * https://stackoverflow.com/questions/15818839/value-annotation-doesnt-return-a-value
   * https://stackoverflow.com/questions/5853167/runnable-with-a-parameter
   * https://www.baeldung.com/java-thread-parameters
   * [Runnable vs. Callable in Java](https://www.baeldung.com/java-runnable-callable)
   * [Quick Guide to Spring Bean Scopes](https://www.baeldung.com/spring-bean-scopes)
   * [Custom Scope in Spring](https://www.baeldung.com/spring-custom-scope)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
