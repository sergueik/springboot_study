### Info

[Logback](https://www.baeldung.com/logback)
[Appender](https://logback.qos.ch/manual/appenders.html)
writing logging messaged into [Windows Event log](https://en.wikipedia.org/wiki/Event_Viewer)
The project uses code converted from one of [java native access](https://github.com/java-native-access/jna)
project contributions - [dblock/log4jna](https://github.com/dblock/log4jna) - the JNA wrapper around the native
Windows Event Log `ReportEvent` [function](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-reporteventa)
that resides  in `advapi32.dll` [dll](https://en.wikipedia.org/wiki/Microsoft_Windows_library_files)
which is system dll hosting misc. security calls and functions for manipulating the Windows Registry
 by replacing the original project depednency on [log4j](https://en.wikipedia.org/wiki/Log4j) - discouraged because of [log4jshell](https://en.wikipedia.org/wiki/Log4Shell)
 and bundling the logger with logback enabling one to configure the logger
 through invoker class / message severity / executing host operating system fashion. The environment test logic relies on 
 embedded [Janino compiler](https://www.janino.net) dependency

### Usage

### Conditional Loging
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
* NOTE: use Java 11, otherwise conditional logging won't work
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

```powershell
remove-eventlog -LogName 'log4jna_sample'
```
* create the log in elevated console. You can specify DOS-style envronment expression in the arguments:
```powershell
$resource_dll_path = '%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
alternatiely can use full path:
```powershell
$resource_dll_path = 'C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
```
or absolute path to local file:
```powershell
$resource_dll_path = (resolve-path 'src\main\resources\Win32EventLogAppender.dll').Path
```
(this variant is only needed for legacy support)
this willcreate an empty event log named `log4jna_sample`
```cmd
wevtutil.exe enum-logs | findstr log4jna_sample
```
```text
log4jna_sample
```
under the hood this install command produces two entries in the Windows Registry for eventlog service:

![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-eventlog-config-before.png)

![Event log Config After](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-eventlog-config-after.png)

```powershell
get-item HKLM:\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample
```
```text

    Hive: HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog

Name                           Property
----                           --------
log4jna_sample                 MaxSize            : 524288
                               AutoBackupLogFiles : 0
```

```
get-childitem HKLM:\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample

```
```text
    Hive: HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample


Name                           Property
----                           --------
example.log4jna_sample         EventMessageFile    : C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
                               CategoryMessageFile : C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
                               CategoryCount       : 0
log4jna_sample                 EventMessageFile    : C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
                               CategoryMessageFile : C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
                               CategoryCount       : 0


```
* run subsequent steps from non-elevated console, as a regular user
* review and update the `src/main/resources/logback.xml` with the  path to CategoryResourceFile if changing using the same value as earlier:
```XML
<configuration>
	<appender name="eventlog" class="example.EventLogAppender">
		<id>42</id>
		<source>example.log4jna_sample</source>
		<application>log4jna_sample</application>

		<resource>%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll</resource>
		<encoder>
			<pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>
		</encoder>
	</appender>
	<appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
		<encoder class="ch.qos.logback.classic.encoder.PatternLayoutEncoder">
			<pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n
			</pattern>
		</encoder>
	</appender>
	<root level="DEBUG">
		<appender-ref ref="CONSOLE" />
	</root>
	<root level="WARN">
		<appender-ref ref="eventlog" />
	</root>
</configuration>

```
the conditional logic part is optional
```XML
	<root level="WARN">
		<appender-ref ref="CONSOLE" />
		<if condition="isDefined(&quot;windir&quot;)">
			<then>
				<appender-ref ref="eventlog" />
			</then>
		</if>
	</root>

```
* build the application jar
```cmd
cd plain
mvn clean package
```

* run the application
```cmd
java -cp target\example.logback-eventlog.jar;target\lib\* example.App the quick brown fox jumps over the dazy log
```

this will print to console:

```text
08:31:26.748 [main] ERROR example.App - message: the quick brown fox jumps over the dazy log
DEBUG: appending event message: 08:31:26.748 [main] ERROR example.App - message: the quick brown fox jumps over the dazy log

Verified EventMessageFile path C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
Verified CategoryMessageFile path C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
08:31:27.013 [main] WARN  example.App - message: the quick brown fox jumps over
the dazy log
DEBUG: appending event message: 08:31:27.013 [main] WARN  example.App - message: the quick brown fox jumps over the dazy log

Verified EventMessageFile path C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dllVerified CategoryMessageFile path C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll

```

and add Windows Event Logs:
![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-message.png)

```powershell
get-eventlog -logname log4jna_sample -newest 2 |format-list
```
```text
Index              : 257
EntryType          : Information
InstanceId         : 4096
Message            : 08:31:27.013 [main] WARN  example.App - message: the quick brown fox jumps over the dazy log

Category           : %1
CategoryNumber     : 3
ReplacementStrings : {08:31:27.013 [main] WARN  example.App - message: the quick brown fox jumps over the dazy log
                     }
Source             : example.log4jna_sample
TimeGenerated      : 4/12/2024 8:31:27 AM
TimeWritten        : 4/12/2024 8:31:27 AM
UserName           :

Index              : 256
EntryType          : Information
InstanceId         : 4096
Message            : 08:31:26.748 [main] ERROR example.App - message: the quick brown fox jumps over the dazy log

Category           : %1
CategoryNumber     : 3
ReplacementStrings : {08:31:26.748 [main] ERROR example.App - message: the quick brown fox jumps over the dazy log
                     }
Source             : example.log4jna_sample
TimeGenerated      : 4/12/2024 8:31:27 AM
TimeWritten        : 4/12/2024 8:31:27 AM
UserName           :

```
or  
```cmd
wevtutil.exe query-events log4jna_sample /rd:true /f:text /c:2
```
reveals
```text
Event[0]:
  Log Name: log4jna_sample
  Source: example.log4jna_sample
  Date: 2024-04-12T08:31:27.000
  Event ID: 4096
  Task: %1
  Level: Information
  Opcode: Info
  Keyword: Classic
  User: N/A
  User Name: N/A
  Computer: sergueik42
  Description: 08:31:27.013 [main] WARN  example.App - message: the quick brown fox jumps over the dazy log


Event[1]:
  Log Name: log4jna_sample
  Source: example.log4jna_sample
  Date: 2024-04-12T08:31:27.000
  Event ID: 4096
  Task: %1
  Level: Information
  Opcode: Info
  Keyword: Classic
  User: N/A
  User Name: N/A
  Computer: sergueik42
  Description: 08:31:26.748 [main] ERROR example.App - message: the quick brown fox jumps over the dazy log

```

for Spring, can do
```cmd
cd  spring
mvn spring-boot:run
```


and interact with app:
```cmd
curl -s  127.0.0.1:8080/example?data=123
```
this will log to curl console
```text
request processed: 123
```
to spring console
```text

Apppending message(3) (success): 15:54:08.482 [http-nio-8080-exec-4] WARN  example.LogHelper - request processed: 123 eventLogType: 4 category:3
Reported Event: 15:54:08.482 [http-nio-8080-exec-4] WARN  example.LogHelper - request processed: 123


```

to event log:
```
Index              : 116
EntryType          : Information
InstanceId         : 42
Message            : 15:54:08.466 [http-nio-8080-exec-4] INFO
                     example.LogHelper - request processed: 123

Category           : %1
CategoryNumber     : 3
ReplacementStrings : {15:54:08.466 [http-nio-8080-exec-4] INFO
                     example.LogHelper - request processed: 123
                     }
Source             : example.log4jna_sample
TimeGenerated      : 4/19/2024 3:54:08 PM
TimeWritten        : 4/19/2024 3:54:08 PM
UserName           :

```
![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-logback-eventlog/screenshots/capture-spring-eventlog.png)

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

### Avoiding Installing the Event Log
To use the `Application` event log which is installed on every system by default
make the following settings:

```powershell
get-childitem HKLM:\SYSTEM\CurrentControlSet\services\eventlog\Application 


```
this will reveal a lot of loggers. narrow it down
```powershell
get-item HKLM:\SYSTEM\CurrentControlSet\services\eventlog\Application\Application

```
```text

    Hive:
    HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\Application


Name                           Property
----                           --------
Application                    CategoryCount       : 7
                               CategoryMessageFile :
                               C:\Windows\system32\wevtapi.dll

```

update the `logback.xml` with
```xml
		<resource>%SystemRoot%\system32\wevtapi.dll</resource>

```
observe the log will silently not be written. Similar attempt through `jna-eventlog` leads to exception
Fixing this is a work in progress

### Logging to Application Event Log

```XML
	<appender name="eventlog" class="example.EventLogAppender">
		<resource>%SystemRoot%\System32\wer.dll</resource>
		<id>1000</id>
		<source>Application Error</source>
		<application>Application</application>
		<encoder>
			<pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n
			</pattern>
		</encoder>
	</appender>

```
```cmd
java -cp target\example.logback-eventlog.jar;target\lib\* example.App the quick brown fox jumps over the lazy frog
```
```text
08:25:05.045 [main] ERROR example.App - message: the quick brown fox jumps over the lazy frog
DEBUG: appending event:

message: "08:25:05.045 [main] ERROR example.App - message: the quick brown fox jumps over the lazy frog"
id: 1000
server: "."
application: "Application Error"
source: "Application"
eventMessageFile: "%SystemRoot%\System32\wer.dll"
categoryMessageFile: "%SystemRoot%\System32\wer.dll"

Apppending message(1): 08:25:05.045 [main] ERROR example.App - message: the quick brown fox jumps over the lazy frog eventLogType: 4 category:3
registerEventSource(1) applicationKeyPath: SYSTEM\CurrentControlSet\Services\EventLog\Application eventSourceKeyPath: SYSTEM\CurrentControlSet\Services\EventLog\Application\Application Error
registerEventSource(2)
registerEventSource(3)
registerEventSource(4)
registerEventSource(5)
Reported Event: 08:25:05.733 [main] ERROR example.App - message: the quick brown fox jumps over the lazy frog

```
```powershell
get-eventlog -source "application error" -logname application -newest 1 |format-list
```
```text
Index              : 94060
EntryType          : Information
InstanceId         : 1000
Message            : Faulting application name: 08:25:05.733 [main] WARN
                     example.App - message: the quick brown fox jumps over the
                     lazy frog
                     , version: %2, time stamp: 0x%3
                     Faulting module name: %4, version: %5, time stamp: 0x%6
                     Exception code: 0x%7
                     Fault offset: 0x%8
                     Faulting process id: 0x%9
                     Faulting application start time: 0x%10
                     Faulting application path: %11
                     Faulting module path: %12
                     Report Id: %13
Category           : (3)
CategoryNumber     : 3
ReplacementStrings : {08:25:05.733 [main] WARN  example.App - message: the
                     quick brown fox jumps over the lazy frog
                     }
Source             : Application Error
TimeGenerated      : 4/19/2024 8:25:05 AM
TimeWritten        : 4/19/2024 8:25:05 AM
UserName           :

```
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
