### Info

This project contains minimal demo code of log4j example. Since the Apache Log4j 1.2 reached end of life in August 2015, the log4j dependency is replaced in this project with a log4j2 one, while illustrating how to keep the configuration file `log4j.xml`

### Usage

#### Testing Locally

* run standalone app
```sh
mvn compile
cp src/main/resources/log4j.xml .
java -Dlog4j.configuration=log4j.xml -cp $HOME/.m2/repository/log4j/log4j/1.2.17/log4j-1.2.17.jar:target/classes example.Basic
```
NOTE: on Windows host will be getting the error:
```cmd
java -Dlog4j.configuration=log4j.xml -cp %userprofile%\.m2\repository\log4j\log4j\1.2.17\log4j-1.2.17.jar;target\classes example.Basic
```
```text
log4j:ERROR Could not parse url [file:/C:/developer/sergueik/springboot_study/basic-log4j/target/classes/log4j.xml]
.java.io.FileNotFoundException: C:\developer\sergueik\springboot_study\basic-log4j\target\classes\log4j2.dtd
 (The system cannot find the file specified)
        at java.io.FileInputStream.open0(Native Method)
        at java.io.FileInputStream.open(FileInputStream.java:195)
        at java.io.FileInputStream.<init>(FileInputStream.java:138)
        at java.io.FileInputStream.<init>(FileInputStream.java:93)
        at sun.net.www.protocol.file.FileURLConnection.connect(FileURLConnection.java:90)
        at sun.net.www.protocol.file.FileURLConnection.getInputStream(FileURLConnection.java:188)
        at com.sun.org.apache.xerces.internal.impl.XMLEntityManager.setupCurrentEntity(XMLEntityManager.java:623)
        at com.sun.org.apache.xerces.internal.impl.XMLEntityManager.startEntity(XMLEntityManager.java:1304)
        at com.sun.org.apache.xerces.internal.impl.XMLEntityManager.startDTDEntity(XMLEntityManager.java:1270)
        at com.sun.org.apache.xerces.internal.impl.XMLDTDScannerImpl.setInputSource(XMLDTDScannerImpl.java:264)
        at com.sun.org.apache.xerces.internal.impl.XMLDocumentScannerImpl$DTDDriver.dispatch(XMLDocumentScannerImpl.java:1161)
        at com.sun.org.apache.xerces.internal.impl.XMLDocumentScannerImpl$DTDDriver.next(XMLDocumentScannerImpl.java:1045)
        at com.sun.org.apache.xerces.internal.impl.XMLDocumentScannerImpl$PrologDriver.next(XMLDocumentScannerImpl.java:959)
        at com.sun.org.apache.xerces.internal.impl.XMLDocumentScannerImpl.next(XMLDocumentScannerImpl.java:602)
        at com.sun.org.apache.xerces.internal.impl.XMLDocumentFragmentScannerImpl.scanDocument(XMLDocumentFragmentScannerImpl.java:505)
        at com.sun.org.apache.xerces.internal.parsers.XML11Configuration.parse(XML11Configuration.java:841)
        at com.sun.org.apache.xerces.internal.parsers.XML11Configuration.parse(XML11Configuration.java:770)
        at com.sun.org.apache.xerces.internal.parsers.XMLParser.parse(XMLParser.java:141)
        at com.sun.org.apache.xerces.internal.parsers.DOMParser.parse(DOMParser.java:243)
        at com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderImpl.parse(DocumentBuilderImpl.java:339)
        at org.apache.log4j.xml.DOMConfigurator$2.parse(DOMConfigurator.java:769)
        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:871)
        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:778)
        at org.apache.log4j.helpers.OptionConverter.selectAndConfigure(OptionConverter.java:526)
        at org.apache.log4j.LogManager.<clinit>(LogManager.java:127)
        at org.apache.log4j.Logger.getLogger(Logger.java:117)
        at example.Basic.<clinit>(Basic.java:7)
log4j:WARN No appenders could be found for logger (example.Basic).
log4j:WARN Please initialize the log4j system properly.
log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.

```
removing the DTD 

```XML
<!DOCTYPE log4j:configuration SYSTEM "log4j2.dtd">
```
makes error worse:
```text
log4j:WARN Continuable parsing error 3 and column 82
log4j:WARN Document root element "log4j:configuration", must match DOCTYPE root"null".
log4j:WARN Continuable parsing error 3 and column 82
log4j:WARN Document is invalid: no grammar found.
log4j: reset attribute= "".
log4j: Threshold ="".
log4j: Level value for root is  [DEBUG].
log4j: root level set to DEBUG
log4j: Class name: [org.apache.log4j2.FileAppender]
log4j:ERROR Could not create an Appender. Reported error follows.
java.lang.ClassNotFoundException: org.apache.log4j2.FileAppender
        at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
        at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:335)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
        at java.lang.Class.forName0(Native Method)
        at java.lang.Class.forName(Class.java:264)
        at org.apache.log4j.helpers.Loader.loadClass(Loader.java:198)
        at org.apache.log4j.xml.DOMConfigurator.parseAppender(DOMConfigurator.java:247)
        at org.apache.log4j.xml.DOMConfigurator.findAppenderByName(DOMConfigurator.java:176)
        at org.apache.log4j.xml.DOMConfigurator.findAppenderByReference(DOMConfigurator.java:191)
        at org.apache.log4j.xml.DOMConfigurator.parseChildrenOfLoggerElement(DOMConfigurator.java:523)
        at org.apache.log4j.xml.DOMConfigurator.parseRoot(DOMConfigurator.java:492)
        at org.apache.log4j.xml.DOMConfigurator.parse(DOMConfigurator.java:1006)

        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:872)
        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:778)
        at org.apache.log4j.helpers.OptionConverter.selectAndConfigure(OptionConverter.java:526)
        at org.apache.log4j.LogManager.<clinit>(LogManager.java:127)
        at org.apache.log4j.Logger.getLogger(Logger.java:117)
        at example.Basic.<clinit>(Basic.java:7)
log4j: Appender named [file] not found.
log4j: Class name: [org.apache.log4j2.ConsoleAppender]
log4j:ERROR Could not create an Appender. Reported error follows.java.lang.ClassNotFoundException: org.apache.log4j2.ConsoleAppender
        at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
        at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:335)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
        at java.lang.Class.forName0(Native Method)
        at java.lang.Class.forName(Class.java:264)
        at org.apache.log4j.helpers.Loader.loadClass(Loader.java:198)
        at org.apache.log4j.xml.DOMConfigurator.parseAppender(DOMConfigurator.java:247)
        at org.apache.log4j.xml.DOMConfigurator.findAppenderByName(DOMConfigurator.java:176)
        at org.apache.log4j.xml.DOMConfigurator.findAppenderByReference(DOMConfigurator.java:191)
        at org.apache.log4j.xml.DOMConfigurator.parseChildrenOfLoggerElement(DOMConfigurator.java:523)
        at org.apache.log4j.xml.DOMConfigurator.parseRoot(DOMConfigurator.java:492)
        at org.apache.log4j.xml.DOMConfigurator.parse(DOMConfigurator.java:1006)

        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:872)
        at org.apache.log4j.xml.DOMConfigurator.doConfigure(DOMConfigurator.java:778)
        at org.apache.log4j.helpers.OptionConverter.selectAndConfigure(OptionConverter.java:526)
        at org.apache.log4j.LogManager.<clinit>(LogManager.java:127)
        at org.apache.log4j.Logger.getLogger(Logger.java:117)
        at example.Basic.<clinit>(Basic.java:7)
log4j: Appender named [console] not found.
log4j:WARN No appenders could be found for logger (example.Basic).
log4j:WARN Please initialize the log4j system properly.
log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.

```
adding the log4j2 jars explicitly
```
java -Dlog4j.configuration=log4j.xml -cp %userprofile%\.m2\repository\org\apache\logging\log4j\log4j-api\2.17.1\log4j-api-2.17.1.jar;%userprofile%\.m2\repository\org\apache\logging\log4j\log4j-core\2.17.1\log4j-core-2.17.1.jar;target\classes example.Basic
```

reveals the code defect - it still refers to log4j:
```text
Exception in thread "main" java.lang.NoClassDefFoundError: org/apache/log4j/Logger
        at example.Basic.<clinit>(Basic.java:7)
Caused by: java.lang.ClassNotFoundException: org.apache.log4j.Logger
        at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
        at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:335)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
        ... 1 more
```

replacing the dependencies reveals the new error:
```text
ERROR StatusLogger Reconfiguration failed: No configuration found for '6d06d69c' at 'null' in 'null'
```
or package
```
mvn package
```
* and run standalone spring boot app
```sh
mvn -Dlog4j.configuration=log4.xml spring-boot:run
```
and check the messages in `logs/App.log` and to console:
```sh
21:35:06.166 [main] INFO  o.s.b.c.e.t.TomcatEmbeddedServletContainer - Tomcat started on port(s): 8080 (http)
```
and in `App.log` only
```sh
[main] INFO  22ogger - init message
[main] INFO  22ogger - init message
[main] WARN  23ogger - init message
```

then
```sh
curl http://localhost:8080/example?data=12345
```
this will be logged:
```sh
```
### Adding Rabbitmq

* copy `example.rabbitmq-appender-0.3.0-SNAPSHOT.jar` from `../basic-rabbitmq-appender/target' to `src/main/resources`
* run standalone basic example
```sh
mvn compile

java -Dlog4j.configuration=log4j.xml -cp $HOME/.m2/repository/log4j/log4j/1.2.17/log4j-1.2.17.jar:src/main/resources/example.rabbitmq-appender-0.3.0-SNAPSHOT.jar:$HOME/.m2/repository/com/rabbitmq/amqp-client/5.8.0/amqp-client-5.8.0.jar:$HOME/.m2/repository/org/slf4j/slf4j-log4j12/1.7.5/slf4j-log4j12-1.7.5.jar:$HOME/.m2/repository/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar:$HOME/.m2/repository/org/json/json/20160810/json-20160810.jar:target/classes example.Basic
```
### See Also
 * [Logging in Spring Boot](https://www.baeldung.com/spring-boot-logging)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

