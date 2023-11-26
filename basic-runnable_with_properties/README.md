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
### See Also:

   * https://stackoverflow.com/questions/15818839/value-annotation-doesnt-return-a-value
   * https://stackoverflow.com/questions/5853167/runnable-with-a-parameter
   * https://www.baeldung.com/java-thread-parameters
   * [Quick Guide to Spring Bean Scopes](https://www.baeldung.com/spring-bean-scopes)
   * [Custom Scope in Spring](https://www.baeldung.com/spring-custom-scope)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
