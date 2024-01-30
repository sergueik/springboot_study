### Info

example from [creating a Custom Logback Appender](https://www.baeldung.com/custom-logback-appender) down level the dependency jars to work under Java 1.8 and few basic modifications to allow encoder and ad hoc parameter

### Usage
```sh
mvn package
```
```sh
java -cp target\custom_logback_appender-0.2.0-SNAPSHOT.jar;target\lib\* example.App
```
this will display
```text
DEBUG: appending event message: 18:20:37.355 [main] INFO  example.App - Example log from App
18:20:37.355 [main] INFO  example.App - Example log from App
DEBUG: appending event message: 18:20:37.388 [main] WARN  mapAppender - Special Example log from App
DEBUG: appending event message: 18:20:37.388 [main] WARN  mapAppender - Special Example log from App
18:20:37.388 [main] WARN  mapAppender - Special Example log from App
```
### See Also

  * Baeldung
    + [A Guide To Logback](https://www.baeldung.com/logback)
    + [Get Log Output in JSON](https://www.baeldung.com/java-log-json-output)
    + [SLF4J Warning: Class Path Contains Multiple SLF4J Bindings](https://www.baeldung.com/slf4j-classpath-multiple-bindings)
    + [Sending Emails with Logback](https://www.baeldung.com/logback-send-email)
    + [Mask Sensitive Data in Logs With Logback](https://www.baeldung.com/logback-mask-sensitive-data)
    + [Creating a Custom Logback Appender](https://www.baeldung.com/custom-logback-appender)
    * [Log framework learning](https://github.com/rookiesnewbie/logback)
    * https://stackoverflow.com/questions/7839565/logging-levels-logback-rule-of-thumb-to-assign-log-levels

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
