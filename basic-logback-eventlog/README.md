### Info

[Logback](https://www.baeldung.com/logback)
[Appender](https://logback.qos.ch/manual/appenders.html)
performing logging messaged into [Event log](https://en.wikipedia.org/wiki/Event_Viewer)
The project uses code
converted from one of
[java native access](https://github.com/java-native-access/jna)
project contributions - [dblock/log4jna](https://github.com/dblock/log4jna) - the JNA wrapper around the native
Windows Event Log `ReportEvent` [function](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-reporteventa)
that resides  in `advapi32.dll` [dll](https://en.wikipedia.org/wiki/Microsoft_Windows_library_files)
which is system dll hosting misc. security calls and functions for manipulating the Windows Registry
 by replacing the original project depednency on [log4j](https://en.wikipedia.org/wiki/Log4j) - discouraged because of [log4jshell](https://en.wikipedia.org/wiki/Log4Shell)
 and bundling the logger with logback enabling one to configure the logger
 through invoker class / message severity / executing host operating system fashion. The environment test logic relies on 
 embedded [Janino compiler](https://www.janino.net) dependency

### Usage

#### Linux

* console only
```sh
mvn package
java -cp target/example.logback-eventlog.jar:target/lib/* example.App the quick brown fox jumps over the lazy dog
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

* remove the custom event log completely

```poweshell
remove-eventlog -logname log4jna_sample
```
* run from elevated console. ([WIP] subsequent runs will work for any user)
to create the event source `log4jna_sample` in  `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx`

copy Microsoft provided dummy event log provider category resource dll into project resource directory if not building from the source:
```cmd
copy /y %windir%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll src\main\resources\Win32EventLogAppender.dll
``` 
build application jar
```cmd
mvn clean package
```

* run from elevated console. ([WIP] subsequent runs will work for any user)

in the first run Java application will observe non existing and create the event source `log4jna_sample` in  `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx`
```cmd
mvn clean package
```
run application
```cmd
java -cp target\example.logback-eventlog.jar;target\lib\* example.App the quick brown fox jumps over the lazy dog
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
NOTE: in `logback.xml` by redefining the `root` logger to `WARN` one loses earlier definition on `INFO` - a more granular configuration is possible

and creates two entries in the Windows Registry for eventlog service:

![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-eventlog-config-before.png)

![Event log Config After](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-eventlog-config-after.png)


and add Windows Event Log:
![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-message.png)

Alternatively remove the event log and create using Microsofr Powershell cmdlets:
```powershell

$logname = 'log4jna_sample'
remove-eventlog -logname $logname

$app = 'log4jna_sample'
$resource_dll = 'C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-EventLog -LogName $logname -source $app -CategoryResourceFile $resource_dll -MessageResourceFile $resource_dll -ParameterResourceFile $resource_dll

```
then run the app in non-elevated prompt:
```cmd
java -cp target\example.logback-eventlog.jar;target\lib\* example.App the quick brown fox jumps over the lazy cat
```
this will log
```
12:07:14.857 [main] ERROR example.App - message: the quick brown fox jumps over the lazy cat
DEBUG: appending event message: 12:07:14.857 [main] ERROR example.App - message: the quick brown fox jumps over the lazy cat

12:07:14.998 [main] WARN  example.App - message: the quick brown fox jumps over the lazy cat
DEBUG: appending event message: 12:07:14.998 [main] WARN  example.App - message: the quick brown fox jumps over the lazy cat
```
```powershell
get-eventlog -logname log4jna_sample -newest 2 |format-list

```
```text
Index              : 36
EntryType          : Information
InstanceId         : 4096
Message            : 12:09:03.299 [main] WARN  example.App - message: the
                     quick brown fox jumps over the lazy cat

Category           : Info
CategoryNumber     : 3
ReplacementStrings : {12:09:03.299 [main] WARN  example.App - message: the
                     quick brown fox jumps over the lazy cat
                     }
Source             : example.log4jna_sample
TimeGenerated      : 3/25/2024 12:09:03 PM
TimeWritten        : 3/25/2024 12:09:03 PM
UserName           :

Index              : 35
EntryType          : Information
InstanceId         : 4096
Message            : 12:09:03.159 [main] ERROR example.App - message: the
                     quick brown fox jumps over the lazy cat

Category           : Info
CategoryNumber     : 3
ReplacementStrings : {12:09:03.159 [main] ERROR example.App - message: the
                     quick brown fox jumps over the lazy cat
                     }
Source             : example.log4jna_sample
TimeGenerated      : 3/25/2024 12:09:03 PM
TimeWritten        : 3/25/2024 12:09:03 PM
UserName           :



```
#### Verify

* Linux
```sh
mvn package
java -cp target/example.logback-eventlog.jar:target/lib/* example.App the quick brown fox jumps over the lazy dog
```
```text
20:02:26,956 |-INFO in ch.qos.logback.classic.LoggerContext[default] - Could NOT find resource [logback-test.xml]
20:02:26,957 |-INFO in ch.qos.logback.classic.LoggerContext[default] - Found resource [logback.xml] at [jar:file:/home/sergueik/src/springboot_study/basic-logback-eventlog/target/example.jna_eventlog.jar!/logback.xml]
20:02:26,971 |-INFO in ch.qos.logback.core.joran.spi.ConfigurationWatchList@4e9ba398 - URL [jar:file:/home/sergueik/src/springboot_study/basic-logback-eventlog/target/example.logback-eventlog.jar!/logback.xml] is not of type file
20:02:27,063 |-INFO in ch.qos.logback.classic.joran.action.ConfigurationAction - debug attribute not set
20:02:27,063 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - About to instantiate appender of type [example.eventlogAppender]
20:02:27,067 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - Naming appender as [map]
20:02:27,075 |-INFO in ch.qos.logback.core.joran.action.NestedComplexPropertyIA - Assuming default type [ch.qos.logback.classic.encoder.PatternLayoutEncoder] for [encoder] property
20:02:27,128 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - About to instantiate appender of type [ch.qos.logback.core.ConsoleAppender]
20:02:27,129 |-INFO in ch.qos.logback.core.joran.action.AppenderAction - Naming appender as [CONSOLE]
20:02:27,132 |-INFO in ch.qos.logback.classic.joran.action.RootLoggerAction - Setting level of ROOT logger to INFO
20:02:27,133 |-INFO in ch.qos.logback.classic.joran.action.LoggerAction - Setting level of logger [eventlogAppender] to DEBUG
20:02:27,133 |-INFO in ch.qos.logback.core.joran.action.AppenderRefAction - Attaching appender named [CONSOLE] to Logger[eventlogAppender]
20:02:27,134 |-ERROR in ch.qos.logback.core.joran.conditional.IfAction - Could not find Janino library on the class path. Skipping conditional processing.
20:02:27,134 |-ERROR in ch.qos.logback.core.joran.conditional.IfAction - See also http://logback.qos.ch/codes.html#ifJanino
20:02:27,135 |-WARN in ch.qos.logback.classic.joran.action.LoggerAction - The object on the top the of the stack is not Logger[eventlogAppender] pushed earlier
20:02:27,135 |-WARN in ch.qos.logback.classic.joran.action.LoggerAction - It is: ch.qos.logback.core.joran.conditional.IfAction
20:02:27,135 |-INFO in ch.qos.logback.classic.joran.action.ConfigurationAction - End of configuration.
20:02:27,136 |-INFO in ch.qos.logback.classic.joran.JoranConfigurator@6d7b4f4c - Registering current configuration as safe fallback point

```
* Windows

```powershell
get-eventlog -logname log4jna_sample -newest 2 |format-list
```
reveals
```text

Index              : 255
EntryType          : Information
InstanceId         : 4096
Message            : 14:58:52.493 [main] WARN  example.App - message: the quick brown fox jumps over the lazy dog

Category           : %1
CategoryNumber     : 3
ReplacementStrings : {14:58:52.493 [main] WARN  example.App - message: the quick brown fox jumps over the lazy dog
                     }
Source             : example.log4jna_sample
TimeGenerated      : 3/23/2024 2:58:52 PM
TimeWritten        : 3/23/2024 2:58:52 PM
UserName           :

Index              : 254
EntryType          : Information
InstanceId         : 4096
Message            : 14:58:52.165 [main] ERROR example.App - message: the quick brown fox jumps over the lazy dog

Category           : %1
CategoryNumber     : 3
ReplacementStrings : {14:58:52.165 [main] ERROR example.App - message: the quick brown fox jumps over the lazy dog
                     }
Source             : example.log4jna_sample
TimeGenerated      : 3/23/2024 2:58:52 PM
TimeWritten        : 3/23/2024 2:58:52 PM
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
04:26:37.314 [main] WARN  eventlogAppender - Event log from App message the quick brown fox jumps over the lazy dog
```
### Cleanup
```cmd
wevtutil.exe clear-log log4jna_sample
```

#### Flexible Logging

in `logback.xml` we allow all message be logged through appended when specific custom  logger `eventlogAppender` is called explicitly:

```XML
 <logger name="eventlogAppender" level="DEBUG">
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
### Custom Event Log Templates
have few disadvantages:

 * complex to author (see snapshot [example](https://github.com/sergueik/powershell_samples/tree/bcbeceaf05bb37927bb8f432d0ce08f73514c751/external/wix/basic-eventlog-source-installer/Resource):
```cpp
;// HEADER SECTION
MessageIdTypedef=DWORD

;// CATEGORY DEFINITIONS
MessageId=1
Language=English
Web service
.

;// MESSAGE DEFINITIONS
MessageId=100
Language=English
Max connections was exceeded.
.
MessageId=101
Language=English
Service utilization: %1
.
```
 * require CPP generation tooling

```cmd
@echo OFF
set TARGET=messages
del %TARGET%.dll %TARGET%.rc %TARGET%.res
REM call "c:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\vcvars32.bat"
call "c:\Program Files\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
REM ignore the ERROR: Cannot determine the location of the VS Common Tools folder. 
set PATH=c:\Program Files\Microsoft Visual Studio 10.0\VC\bin;c:\Program Files\Microsoft SDKs\Windows\v7.1\Bin;%PATH%
mc.exe -u %TARGET%.mc
rc.exe -r -fo %TARGET%.res %TARGET%.rc
link.exe -dll -noentry -out:%TARGET%.dll %TARGET%.res /MACHINE:X86
REM Alternatively
c:\Windows\Microsoft.NET\Framework\v4.0.30319\csc.exe /win32res:%TARGET%.res /unsafe /target:library /out:.\%TARGET%_csc.dll
xcopy.exe /Y %TARGET%.dll ..\Setup
```
 * are barely used if at all to style the messages

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




