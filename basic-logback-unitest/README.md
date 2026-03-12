### Info

### Troubleshooting
```text
java.lang.ClassCastException: class org.apache.logging.slf4j.Log4jLogger cannot be cast to class ch.qos.logback.classic.Logger (org.apache.logging.slf4j.Log4jLogger and ch.qos.logback.classic.Logger are in unnamed module of loader 'app')
```
workaround: comment out the log4j2 dependency from `pom.xml`
### See Also

  * https://www.baeldung.com/java-ebcdic-ascii-conversion
  * https://en.wikipedia.org/wiki/EBCDIC

### Author

* [Serguei Kouzmine](kouzmine_serguei@yahoo.com)
