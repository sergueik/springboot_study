### Info

This directory contains basic example from
[repo](https://github.com/eugenp/tutorials/blob/master/core-java-modules/core-java-concurrency-basic/src/main/java/com/baeldung/concurrent) of the
__Implementing a Runnable vs Extending a Thread__ [article](https://www.baeldung.com/java-runnable-vs-extending-thread)
### Usage

```sh
mvn spring-boot:run
```
this logs

```text
16:40:59.707 [pool-1-thread-1] INFO example.task.EventLoggingTask - Value #1 is 0
```

NOTE, the property value was not read through annotation:
```java
@Value("${setting.value:123}")
private long value1;
String message1 = String.format("Value: %d", value1);
	logger.info("Message #1 is {}", message1);
```

### Note:

Uncommenting the value
```java
@Autowired
private Properties properties;
```

in the class 

and running the app

```sh
mvn spring-boot:run
```

shows properteis value to not  be initialized:

```text	
16:44:24.130 [pool-1-thread-1] INFO example.task.EventLoggingTask - Value #1 is 0
16:44:24.134 [pool-1-thread-1] INFO example.task.EventLoggingTask - Exception java.lang.NullPointerException
```

### See Also:

   * https://stackoverflow.com/questions/5853167/runnable-with-a-parameter
   * https://www.baeldung.com/java-thread-parameters

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
