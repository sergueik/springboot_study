### Info

replica of [yokra9/log4jna_samle](https://github.com/yokra9/log4jna_samle) with added missing jna dependency and other minor fixes

### Usage

the first run of the test will pass only when run from elevated console. subsequent runs will work for any user
It will create the event source `log4jna_sample` which is mapped to `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx` owned by `LOCAL SERVICE` built-in account
which ACL is inherired from the directory and limited to `System` and `Administrators`:

```text
C:\Windows\System32\Winevt\Logs\log4jna_sample.evtx NT SERVICE\EventLog:(ID)F
                                                    NT AUTHORITY\СИСТЕМА:(ID)F
                                                    BUILTIN\Администраторы:(ID)F
```


```sh
mvn -Dmaven.test.skip=true package
```
```sh
java  -cp target\log4jna_sample-1.0-SNAPSHOT.jar;target\lib\* com.github.yokra9.log4jna_sample.App "test message"
```
```text
[FATAL] 2024-01-14 04:28:36.133 [main] log4jna_sample.Logging - test message
[ERROR] 2024-01-14 04:28:36.336 [main] log4jna_sample.Logging - test message
[WARN] 2024-01-14 04:28:36.352 [main] log4jna_sample.Logging - test message
[INFO] 2024-01-14 04:28:36.352 [main] log4jna_sample.Logging - test message
[DEBUG] 2024-01-14 04:28:36.367 [main] log4jna_sample.Logging - test message
[TRACE] 2024-01-14 04:28:36.367 [main] log4jna_sample.Logging - test message
```
the logging does not require the user to be elevated. In the absence of the custom event log file, the code will throw `java.lang.RuntimeException` exception
```text
2024-01-15 12:14:02,182 main ERROR An exception occurred processing Appender EventViewer-Appender java.lang.RuntimeException: Could not register event source.
        at org.dblock.log4jna.nt.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:235)
        at org.dblock.log4jna.nt.Win32EventLogAppender.append(Win32EventLogAppender.java:253)
```
the command
```powershell
get-eventlog -logname log4jna_sample -newest 5 |format-list
```
shows
```text

Index              : 18
EntryType          : 0
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message

Category           : Fatal
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message
                     }
Source             : com.github.yokra9.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :

Index              : 17
EntryType          : 0
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message

Category           : Fatal
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message
                     }
Source             : com.github.yokra9.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :

Index              : 16
EntryType          : Information
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message
                     }
Source             : com.github.yokra9.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :

Index              : 15
EntryType          : Warning
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message
                     }
Source             : com.github.yokra9.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :

Index              : 14
EntryType          : Error
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.Logging
                     Message: test message
                     }
Source             : com.github.yokra9.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :


```

NOTE: repackaged jar needs work. the logger fails:
```sh
java  -cp target\SWTsample.jar com.github.yokra9.log4jna_sample.App
```
```text
2024-01-14 04:24:24,523 main ERROR Error processing element Win32EventLog ([Appenders: null]): CLASS_NOT_FOUND
2024-01-14 04:24:24,711 main ERROR Unable to locate appender "EventViewer-Appender" for logger config "root"
[FATAL] 2024-01-14 04:24:24.899 [main] log4jna_sample.Logging -
[ERROR] 2024-01-14 04:24:24.899 [main] log4jna_sample.Logging -
[WARN] 2024-01-14 04:24:24.899 [main] log4jna_sample.Logging -
[INFO] 2024-01-14 04:24:24.899 [main] log4jna_sample.Logging -
[DEBUG] 2024-01-14 04:24:24.914 [main] log4jna_sample.Logging -
[TRACE] 2024-01-14 04:24:24.914 [main] log4jna_sample.Logging -
```
```cmd
cd src\main\resources
wevtutil.exe im sample_log.man
```
(does not create event log)
run the following from project root in elevated powershell prompt.
Use the full package name of the logger
```powershell
get-eventlog log4jna_sample
remove-eventlog -logname log4jna_sample
new-eventLog -logName log4jna_sample -Source 'com.github.yokra9.log4jna_sample' -CategoryResourceFile ((resolve-path 'src\main\resources\Win32EventLogAppender.dll').Path) -MessageResourceFile ((resolve-path 'src\main\resources\Win32EventLogAppender.dll').Path)
```
this will create 
```txt
c:\Windows\System32\winevt\Logs\log4jna_sample.evtx
```
and add registry keys `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample\com.github.yokra9.log4jna_sample`
and `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample\log4jna_sample` in `REG_EXPAND_SZ` value data type,

with absolute path to `Win32EventLogAppender.dll` in `EventMessageFile` and `CategoryMessageFile` values
so presumably one can run the above command with a dummy value `%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll`

```powershell
remove-eventlog -logname log4jna_sample
$resource_dll_path = '%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName log4jna_sample -Source 'com.github.yokra9.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
but after this is done, the java code shows the same`java.lang.RuntimeException` exception attempting to register event source in runtime


alternatively run the jar once in elevated prompt
```
java -cp target\log4jna_sample-1.0-SNAPSHOT.jar;target\lib\* com.github.yokra9.log4jna_sample.App
```
### NOTE

added replica of [dblock/log4jna](https://github.com/dblock/log4jna) at commit `032ee6f` (2.0 release)
### See Also


 
  * [blog covering eventlog logback](http://ykchee.blogspot.com/2012/09/logback-nt-event-log-appender.html) - code unavailable
  * [guide to Java Logging with Logback](https://betterstack.com/community/guides/logging/java/logback/)
  * https://logback.qos.ch/manual/appenders.html
  * [guide To Logback](https://www.baeldung.com/logback)
  * https://learn.microsoft.com/en-us/windows/win32/wes/identifying-the-provider?redirectedfrom=MSDN

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


