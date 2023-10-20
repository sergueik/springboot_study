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

shows neither the value deifned in `application.properties` 
```java
setting.value = 42
```
nor the default value is being assigned to the property
```java
@Value("${setting.value:101}")
	private long value1;
```
 in the `EventLoggingTask` class implementing `Runnable`:

```text
[INFO] Attaching agents: []
10:03:41.780 [pool-1-thread-1] INFO example.task.EventLoggingTask - Attempt to load value1 through annotation: 0
10:03:41.788 [pool-1-thread-1] INFO example.task.EventLoggingTask - Read  value2 is from resource "/application.properties" within application: 42
```

Attempt to autowire the properties object:
```java
@Autowired
private Properties properties;
```

```java
try {
	final String value2 = properties.getProperty("setting.value");
	logger.info("Attempt to load Value from autowired properties object: {}",
	value2);
	} catch (Exception e) {
	logger.info("Exception " + e.toString());
}
```

is logged as NPE:

```text
0:11:05.836 [pool-1-thread-1] INFO example.task.EventLoggingTask - Exception jaa.lang.NullPointerException
```
### See Also:

   * https://stackoverflow.com/questions/15818839/value-annotation-doesnt-return-a-value
   * https://stackoverflow.com/questions/5853167/runnable-with-a-parameter
   * https://www.baeldung.com/java-thread-parameters

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
