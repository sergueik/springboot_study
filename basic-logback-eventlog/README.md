### Info

Logback Appender performing Event log logging converted from [dblock/log4jna](https://github.com/dblock/log4jna)

### Usage

#### Linux

* console only
```sh
mvn package
java -cp target/example.jna_eventlog.jar:target/lib/* example.App the quick brown fox jumps over the lazy dog
```
```text
00:24:59.001 [main] ERROR example.App - message: the quick brown fox jumps over the lazy dog
00:24:59.007 [main] WARN  example.App - message: the quick brown fox jumps over the lazy dog
```
#### Windows

* clear the log
```cmd
wevtutil.exe clear-log log4jna_sample
```
ignore the warning
```text
Failed to clear log log4jna_sample. The specified channel could not be found. Check channel configuration.
```
which will be printed if the log is already deleted / not yet created

* run from elevated console. ([WIP] subsequent runs will work for any user)
to create the event source `log4jna_sample` in  `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx`
```cmd
mvn clean package
```
```cmd
java -cp target\example.jna_eventlog.jar;target\lib\* example.App the quick brown fox jumps over the lazy dog
```

this will print to console:

```text
16:26:57.277 [main] ERROR example.App - message: the quick brown fox jumps over
the lazy dog
DEBUG: appending event message: 16:26:57.277 [main] ERROR example.App - message:
 the quick brown fox jumps over the lazy dog

16:26:58.105 [main] WARN  example.App - message: the quick brown fox jumps over
the lazy dog
DEBUG: appending event message: 16:26:58.105 [main] WARN  example.App - message:
 the quick brown fox jumps over the lazy dog

```
create two entries in the Windows Registry for eventlog service:

![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-eventlog-config-before.png)

![Event log Config After](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-eventlog-config-after.png)


and add Windows Event Log:
![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

#### Verify

* Linux
```sh
mvn package
java -cp target/example.jna_eventlog.jar:target/lib/* example.App the quick brown fox jumps over the lazy dog
```
```text
20:02:26,956 |-INFO in ch.qos.logback.classic.LoggerContext[default] - Could NOT find resource [logback-test.xml]
20:02:26,957 |-INFO in ch.qos.logback.classic.LoggerContext[default] - Found resource [logback.xml] at [jar:file:/home/sergueik/src/springboot_study/basic-logback-eventlog/target/example.jna_eventlog.jar!/logback.xml]
20:02:26,971 |-INFO in ch.qos.logback.core.joran.spi.ConfigurationWatchList@4e9ba398 - URL [jar:file:/home/sergueik/src/springboot_study/basic-logback-eventlog/target/example.jna_eventlog.jar!/logback.xml] is not of type file
20:02:27,063 |-INFO in ch.qos.logback.classic.joran.action.ConfigurationAction - debug attribute not set
20:02:27,063 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - About to instantiate appender of type [example.MapAppender]
20:02:27,067 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - Naming appender as [map]
20:02:27,075 |-INFO in ch.qos.logback.core.joran.action.NestedComplexPropertyIA - Assuming default type [ch.qos.logback.classic.encoder.PatternLayoutEncoder] for [encoder] property
20:02:27,128 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - About to instantiate appender of type [ch.qos.logback.core.ConsoleAppender]
20:02:27,129 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - Naming appender as [CONSOLE]
20:02:27,132 |-INFO in ch.qos.logback.classic.joran.action.RootLoggerAction - Setting level of ROOT logger to INFO
20:02:27,133 |-INFO in ch.qos.logback.classic.joran.action.LoggerAction - Setting level of logger [mapAppender] to DEBUG
20:02:27,133 |-INFO in ch.qos.logback.core.joran.action.AppenderRefAction - Attaching appender named [CONSOLE] to Logger[mapAppender]
20:02:27,134 |-ERROR in ch.qos.logback.core.joran.conditional.IfAction - Could not find Janino library on the class path. Skipping conditional processing.
20:02:27,134 |-ERROR in ch.qos.logback.core.joran.conditional.IfAction - See also http://logback.qos.ch/codes.html#ifJanino
20:02:27,135 |-WARN in ch.qos.logback.classic.joran.action.LoggerAction - The object on the top the of the stack is not Logger[mapAppender] pushed earlier
20:02:27,135 |-WARN in ch.qos.logback.classic.joran.action.LoggerAction - It is: ch.qos.logback.core.joran.conditional.IfAction
20:02:27,135 |-INFO in ch.qos.logback.classic.joran.action.ConfigurationAction - End of configuration.
20:02:27,136 |-INFO in ch.qos.logback.classic.joran.JoranConfigurator@6d7b4f4c - Registering current configuration as safe fallback point

```
* Windows

```powershell
get-eventlog -logname log4jna_sample -newest 1 |format-list
```
reveals
```text
Index              : 32
EntryType          : Information
InstanceId         : 4096
Message            : 04:26:37.314 [main] WARN  mapAppender - Event log from App
                      message the quick brown fox jumps over the lazy dog

Category           : Info
CategoryNumber     : 3
ReplacementStrings : {04:26:37.314 [main] WARN  mapAppender - Event log from Ap
                     p message the quick brown fox jumps over the lazy dog
                     }
Source             : example.log4jna_sample
TimeGenerated      : 31.01.2024 4:26:39
TimeWritten        : 31.01.2024 4:26:39
UserName           :
```

or  
```cmd
wevtutil.exe query-events log4jna_sample /rd:true /f:text /c:1
```
reveals
```text
Event[0]:
  Log Name: log4jna_sample
  Source: example.log4jna_sample
  Date: 2024-01-31T04:26:39.0170000Z
  Event ID: 4096
  Task: Info
  Level: Сведения
  Opcode: Сведения
  Keyword: Классический
  User: N/A
  User Name: N/A
  Computer: DESKTOP-82V9KDO
  Description:
04:26:37.314 [main] WARN  mapAppender - Event log from App message the quick brown fox jumps over the lazy dog
```
### Cleanup
```cmd
wevtutil.exe clear-log log4jna_sample
```

#### Flexible Logging

in `logback.xml` we allow all message be logged through appended when specific custom  logger `mapAppender` is called explicitly:

```XML
 <logger name="mapAppender" level="DEBUG">
  </logger>
```

and for a default log 

```java

```
`WARN` logs on Windows be logged to __System Event Log__:
```xml
 <root level="WARN">
    <appender-ref ref="CONSOLE"/>
    <if condition="isDefined(&quot;windir&quot;)">
      <then>
        <appender-ref ref="map"/>
      </then>
    </if>
  </root>

```

on Unix the `WARN` level will be printed to console.

### NOTE

The logback levels are, in order of precedence: 

  * `TRACE`
  * `DEBUG`
  * `INFO`
  * `WARN`
  * `ERROR`

### See Also

  * https://www.baeldung.com/logback
  * http://janino-compiler.github.io/janino/
  * https://stackoverflow.com/questions/1975939/read-environment-variables-from-logback-configuration-file
  * https://stackoverflow.com/questions/42370870/else-if-in-janino-logback-configuration
  * https://stackoverflow.com/questions/27814140/conditions-in-logback-configuration
  * https://www.wlangiewicz.com/2019/03/28/effective-way-of-using-conditional-expressions-in-logback/
  * https://stackoverflow.com/questions/15911303/how-can-i-configure-logback-conditionally-based-on-context-name
  * https://dennis-xlc.gitbooks.io/the-logback-manual/content/en/chapter-3-configuration/configuration-file-syntax/variable-substitution.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)




