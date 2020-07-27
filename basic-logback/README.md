### Info

This project contains [minimal demo code of logback example](http://logback.qos.ch/manual/appenders.html)

### Usage

* test application locally
```sh
mvn clean package
java -cp target\logback-0.0.1-SNAPSHOT.jar;target\lib\*;target\conf example.Example
```
Note: it does not appear that logback's `RollingFileAppender` [class](https://github.com/qos-ch/logback/blob/master/logback-core/src/main/java/ch/qos/logback/core/rolling/RollingFileAppender.java)
supports configuring log file permissions.

### See Also

 * https://www.codingame.com/playgrounds/4497/configuring-logback-with-spring-boot


### License
This project is licensed under the terms of the MIT license.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


