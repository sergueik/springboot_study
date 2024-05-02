### Info

replica of [yokra9/log4jna_samle](https://github.com/yokra9/log4jna_samle) with added missing jna dependency and other minor fixes

### Usage

* test must be run in elevated console
```cmd
remove-eventlog -logname log4jna_sample
```
* NOTE: for test compiling on Linux, will need to skip tests explicitly as in:
```sh
mvn -Dmaven.test.skip=true package
```
```cmd
mvn clean test
```
if this is not followed observe the errors
```text
2024-05-01 19:14:02,603 main ERROR An exception occurred processing Appender EventLog java.lang.RuntimeException: Could not register event source.
	at example.log4j.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:253)
	at example.log4j.Win32EventLogAppender.append(Win32EventLogAppender.java:292)
	at example.log4j.Win32EventLogAppender.append(Win32EventLogAppender.java:286)
	at org.apache.logging.log4j.core.config.AppenderControl.tryCallAppender(AppenderControl.java:161)
	at org.apache.logging.log4j.core.config.AppenderControl.callAppender0(AppenderControl.java:134)
	at org.apache.logging.log4j.core.config.AppenderControl.callAppenderPreventRecursion(AppenderControl.java:125)
	at org.apache.logging.log4j.core.config.AppenderControl.callAppender(AppenderControl.java:89)
	at org.apache.logging.log4j.core.config.LoggerConfig.callAppenders(LoggerConfig.java:542)
	at org.apache.logging.log4j.core.config.LoggerConfig.processLogEvent(LoggerConfig.java:500)
	at org.apache.logging.log4j.core.config.LoggerConfig.log(LoggerConfig.java:483)
	at org.apache.logging.log4j.core.config.LoggerConfig.log(LoggerConfig.java:417)
	at org.apache.logging.log4j.core.config.AwaitCompletionReliabilityStrategy.log(AwaitCompletionReliabilityStrategy.java:82)
	at org.apache.logging.log4j.core.Logger.log(Logger.java:161)
	at org.apache.logging.log4j.spi.AbstractLogger.tryLogMessage(AbstractLogger.java:2205)
	at org.apache.logging.log4j.spi.AbstractLogger.logMessageTrackRecursion(AbstractLogger.java:2159)
	at org.apache.logging.log4j.spi.AbstractLogger.logMessageSafely(AbstractLogger.java:2142)
	at org.apache.logging.log4j.spi.AbstractLogger.logMessage(AbstractLogger.java:2017)
	at org.apache.logging.log4j.spi.AbstractLogger.logIfEnabled(AbstractLogger.java:1983)
	at org.apache.logging.log4j.spi.AbstractLogger.info(AbstractLogger.java:1320)
	at example.LoggingTest.info(LoggingTest.java:95)
	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.lang.reflect.Method.invoke(Method.java:498)
	at org.junit.platform.commons.util.ReflectionUtils.invokeMethod(ReflectionUtils.java:725)
	at org.junit.jupiter.engine.execution.MethodInvocation.proceed(MethodInvocation.java:60)
	at org.junit.jupiter.engine.execution.InvocationInterceptorChain$ValidatingInvocation.proceed(InvocationInterceptorChain.java:131)
	at org.junit.jupiter.engine.extension.TimeoutExtension.intercept(TimeoutExtension.java:149)
	at org.junit.jupiter.engine.extension.TimeoutExtension.interceptTestableMethod(TimeoutExtension.java:140)
	at org.junit.jupiter.engine.extension.TimeoutExtension.interceptTestMethod(TimeoutExtension.java:84)
	at org.junit.jupiter.engine.execution.ExecutableInvoker$ReflectiveInterceptorCall.lambda$ofVoidMethod$0(ExecutableInvoker.java:115)
	at org.junit.jupiter.engine.execution.ExecutableInvoker.lambda$invoke$0(ExecutableInvoker.java:105)
	at org.junit.jupiter.engine.execution.InvocationInterceptorChain$InterceptedInvocation.proceed(InvocationInterceptorChain.java:106)
	at org.junit.jupiter.engine.execution.InvocationInterceptorChain.proceed(InvocationInterceptorChain.java:64)
	at org.junit.jupiter.engine.execution.InvocationInterceptorChain.chainAndInvoke(InvocationInterceptorChain.java:45)
	at org.junit.jupiter.engine.execution.InvocationInterceptorChain.invoke(InvocationInterceptorChain.java:37)
	at org.junit.jupiter.engine.execution.ExecutableInvoker.invoke(ExecutableInvoker.java:104)
	at org.junit.jupiter.engine.execution.ExecutableInvoker.invoke(ExecutableInvoker.java:98)
	at org.junit.jupiter.engine.descriptor.TestMethodTestDescriptor.lambda$invokeTestMethod$7(TestMethodTestDescriptor.java:214)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.jupiter.engine.descriptor.TestMethodTestDescriptor.invokeTestMethod(TestMethodTestDescriptor.java:210)
	at org.junit.jupiter.engine.descriptor.TestMethodTestDescriptor.execute(TestMethodTestDescriptor.java:135)
	at org.junit.jupiter.engine.descriptor.TestMethodTestDescriptor.execute(TestMethodTestDescriptor.java:66)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$6(NodeTestTask.java:151)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$8(NodeTestTask.java:141)
	at org.junit.platform.engine.support.hierarchical.Node.around(Node.java:137)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$9(NodeTestTask.java:139)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.executeRecursively(NodeTestTask.java:138)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.execute(NodeTestTask.java:95)
	at java.util.ArrayList.forEach(ArrayList.java:1255)
	at org.junit.platform.engine.support.hierarchical.SameThreadHierarchicalTestExecutorService.invokeAll(SameThreadHierarchicalTestExecutorService.java:41)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$6(NodeTestTask.java:155)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$8(NodeTestTask.java:141)
	at org.junit.platform.engine.support.hierarchical.Node.around(Node.java:137)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$9(NodeTestTask.java:139)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.executeRecursively(NodeTestTask.java:138)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.execute(NodeTestTask.java:95)
	at java.util.ArrayList.forEach(ArrayList.java:1255)
	at org.junit.platform.engine.support.hierarchical.SameThreadHierarchicalTestExecutorService.invokeAll(SameThreadHierarchicalTestExecutorService.java:41)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$6(NodeTestTask.java:155)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$8(NodeTestTask.java:141)
	at org.junit.platform.engine.support.hierarchical.Node.around(Node.java:137)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.lambda$executeRecursively$9(NodeTestTask.java:139)
	at org.junit.platform.engine.support.hierarchical.ThrowableCollector.execute(ThrowableCollector.java:73)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.executeRecursively(NodeTestTask.java:138)
	at org.junit.platform.engine.support.hierarchical.NodeTestTask.execute(NodeTestTask.java:95)
	at org.junit.platform.engine.support.hierarchical.SameThreadHierarchicalTestExecutorService.submit(SameThreadHierarchicalTestExecutorService.java:35)
	at org.junit.platform.engine.support.hierarchical.HierarchicalTestExecutor.execute(HierarchicalTestExecutor.java:57)
	at org.junit.platform.engine.support.hierarchical.HierarchicalTestEngine.execute(HierarchicalTestEngine.java:54)
	at org.junit.platform.launcher.core.EngineExecutionOrchestrator.execute(EngineExecutionOrchestrator.java:107)
	at org.junit.platform.launcher.core.EngineExecutionOrchestrator.execute(EngineExecutionOrchestrator.java:88)
	at org.junit.platform.launcher.core.EngineExecutionOrchestrator.lambda$execute$0(EngineExecutionOrchestrator.java:54)
	at org.junit.platform.launcher.core.EngineExecutionOrchestrator.withInterceptedStreams(EngineExecutionOrchestrator.java:67)
	at org.junit.platform.launcher.core.EngineExecutionOrchestrator.execute(EngineExecutionOrchestrator.java:52)
	at org.junit.platform.launcher.core.DefaultLauncher.execute(DefaultLauncher.java:114)
	at org.junit.platform.launcher.core.DefaultLauncher.execute(DefaultLauncher.java:86)
	at org.junit.platform.launcher.core.DefaultLauncherSession$DelegatingLauncher.execute(DefaultLauncherSession.java:86)
	at org.junit.platform.launcher.core.SessionPerRequestLauncher.execute(SessionPerRequestLauncher.java:53)
	at org.apache.maven.surefire.junitplatform.JUnitPlatformProvider.invokeAllTests(JUnitPlatformProvider.java:142)
	at org.apache.maven.surefire.junitplatform.JUnitPlatformProvider.invoke(JUnitPlatformProvider.java:117)
	at org.apache.maven.surefire.booter.ForkedBooter.invokeProviderInSameClassLoader(ForkedBooter.java:383)
	at org.apache.maven.surefire.booter.ForkedBooter.runSuitesInProcess(ForkedBooter.java:344)
	at org.apache.maven.surefire.booter.ForkedBooter.execute(ForkedBooter.java:125)
	at org.apache.maven.surefire.booter.ForkedBooter.main(ForkedBooter.java:417)
Caused by: com.sun.jna.platform.win32.Win32Exception: Access is denied.
	at com.sun.jna.platform.win32.Advapi32Util.registryCreateKey(Advapi32Util.java:1282)
	at com.sun.jna.platform.win32.Advapi32Util.registryCreateKey(Advapi32Util.java:1260)
	at example.log4j.Win32EventLogAppender.createAndSetAllKeys(Win32EventLogAppender.java:372)
	at example.log4j.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:351)
	at example.log4j.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:250)
	... 88 more
```
after the test will observe the following event log messages:
```powershell
get-eventlog -logname log4jna_sample -newest 5 |format-list
```
```text
Index              : 30
EntryType          : 0
InstanceId         : 4096
Message            : Thread: main
                     Logger: example.LoggingTest
                     Message: trace

Category           : Fatal
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: example.LoggingTest
                     Message: trace
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/1/2024 3:03:32 PM
TimeWritten        : 5/1/2024 3:03:32 PM
UserName           :

Index              : 29
EntryType          : Error
InstanceId         : 4096
Message            : Thread: main
                     Logger: example.LoggingTest
                     Message: fatal

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: example.LoggingTest
                     Message: fatal
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/1/2024 3:03:32 PM
TimeWritten        : 5/1/2024 3:03:32 PM
UserName           :

Index              : 28
EntryType          : Error
InstanceId         : 4096
Message            : Thread: main
                     Logger: example.LoggingTest
                     Message: error

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: example.LoggingTest
                     Message: error
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/1/2024 3:03:32 PM
TimeWritten        : 5/1/2024 3:03:32 PM
UserName           :

Index              : 27
EntryType          : 0
InstanceId         : 4096
Message            : Thread: main
                     Logger: example.LoggingTest
                     Message: debug

Category           : Fatal
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: example.LoggingTest
                     Message: debug
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/1/2024 3:03:32 PM
TimeWritten        : 5/1/2024 3:03:32 PM
UserName           :

Index              : 26
EntryType          : Warning
InstanceId         : 4096
Message            : Thread: main
                     Logger: example.LoggingTest
                     Message: warn

Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: example.LoggingTest
                     Message: warn
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/1/2024 3:03:32 PM
TimeWritten        : 5/1/2024 3:03:32 PM
UserName           :

```
* the first run of the test will pass only when run from elevated console. subsequent runs will work for any user
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
java  -cp target\log4jna_sample-4.0-SNAPSHOT.jar;target\lib\* example.log4jna_sample.App "test message"
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
Source             : example.log4jna_sample
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
Source             : example.log4jna_sample
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
Source             : example.log4jna_sample
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
Source             : example.log4jna_sample
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
Source             : example.log4jna_sample
TimeGenerated      : 1/15/2024 12:15:40 PM
TimeWritten        : 1/15/2024 12:15:40 PM
UserName           :


```

NOTE: repackaged jar needs work. the logger fails:
```sh
java  -cp target\SWTsample.jar example.log4jna_sample.App
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
$resource_dll_path = (resolve-path 'src\main\resources\Win32EventLogAppender.dll').Path
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
this will create 
```txt
c:\Windows\System32\winevt\Logs\log4jna_sample.evtx
```
and add registry keys `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample\example.log4jna_sample`
and `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\eventlog\log4jna_sample\log4jna_sample` in `REG_EXPAND_SZ` value data type,

with absolute path to `Win32EventLogAppender.dll` in `EventMessageFile` and `CategoryMessageFile` values
so presumably one can run the above command with a dummy value `%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll`

```powershell
remove-eventlog -logname log4jna_sample
$resource_dll_path = 'C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
but after this is done, the java code shows the same`java.lang.RuntimeException` exception attempting to register event source in runtime

the fix to make the `log4j2.xml` `dllfile` property look the same does not work:

```XML
<Property name="dllfile">%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll</Property>
```

After adding a dummy event to the `log4jna_sample` log and 
```cmd
wevtutil.exe  gl log4jna_sample
```
```text
name: 	
enabled: true
type: Admin
owningPublisher:
isolation: Application
channelAccess: O:BAG:SYD:(A;;0xf0007;;;SY)(A;;0x7;;;BA)(A;;0x7;;;SO)(A;;0x3;;;IU)(A;;0x3;;;SU)(A;;0x3;;;S-1-5-3)(A;;0x3;;;S-1-5-33)(A;;0x1;;;S-1-5-32-573)
logging:
  logFileName: %SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx
  retention: false
  autoBackup: false
  maxSize: 1052672
publishing:
  fileMax: 1
```

```cmd
wevtutil.exe ep | where-object { $_ -match '.*log4jna_sample'
```
```text
example.log4jna_sample
log4jna_sample
log4jna_sample

```

```cmd
wevtutil.exe gp log4jna_sample
```
```text

name: log4jna_sample
guid: 55748f35-b4f6-4544-8143-6b51970de865
helpLink:
resourceFileName: Win32EventLogAppender.dll
messageFileName: Win32EventLogAppender.dll
```
The test will pass in non-elevated prompt
```cmd
wevtutil.exe qe log4jna_sample /c:1


```
```XML
<Event xmlns='http://schemas.microsoft.com/win/2004/08/events/event'><System><Provider Name='example.log4jna_sample'/><EventID Qualifiers='0'>4096</EventID><Level>4</Level><Task>1</Task><Keywords>0x80000000000000</Keywords><TimeCreated SystemTime='2024-01-15T22:58:19.000000000Z'/><EventRecordID>157</EventRecordID><Channel>log4jna_sample</Channel><Computer>sergueik42</Computer><Security/></System><EventData><Data>Thread: main Logger: log4jna_sample.LoggingTest Message: info</Data>
</EventData></Event>
```

alternatively run the jar once in elevated prompt
```cmd
java -cp target\log4jna_sample-1.0-SNAPSHOT.jar;target\lib\* example.log4jna_sample.App
```
the test works but the event log messages become far less useful:

```text
The description for Event ID 4096 from source example.log4jna_sample cannot be found. Either the component that raises this event is not installed on your local computer or the installation is corrupted. You can install or repair the component on the local computer.

If the event originated on another computer, the display information had to be saved with the event.

The following information was included with the event: 

Thread: main
Logger: log4jna_sample.App
Message: test message 


the message resource is present but the message is not found in the string/message table

```

```text
Category           : (1)
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message
                     }
Source             : example.log4jna_sample
TimeGenerated      : 1/15/2024 2:25:21 PM
TimeWritten        : 1/15/2024 2:25:21 PM
UserName           :

Index              : 140
EntryType          : Error
InstanceId         : 4096
Message            : The description for Event ID '4096' in Source
                     'example.log4jna_sample' cannot be found.  The local
                     computer may not have the necessary registry information
                     or message DLL files to display the message, or you may
                     not have permission to access them.  The following
                     information is part of the event:'Thread: main
                     Logger: log4jna_sample.App
                     Message: test message
                     '
```
NOTE: the  Message id 4096 is hard coded in `Win32EventLogAppender.java`:
```java
final int messageID = 0x1000;
```
therefore using `C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll` which does not have it in its message table is not helpful though providing the absolute path in
```XML
 <Property name="dllfile">src\main\resources\Win32EventLogAppender.dll</Property>
```

appeaars to unblock event log messaging by non elevated user.

### NOTE

added replica of [dblock/log4jna](https://github.com/dblock/log4jna) at commit `032ee6f` (2.0 release)
that is calling `ReportEvent` [method](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-reporteventw) from `advapi32.dll`

[passing](https://github.com/dblock/log4jna/blob/master/log4jna-api/src/main/java/org/dblock/log4jna/nt/Win32EventLogAppender.java#L252) 
the `org.apache.logging.log4j.core.LogEvent` [interface](https://logging.apache.org/log4j/2.x/javadoc/log4j-core/org/apache/logging/log4j/core/LogEvent.html)


### Building Custom Log4JNA

when building the [dblock/log4jna](https://github.com/dblock/log4jna) on a vanilla Windows machine will likely get the error

```text
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-antrun-plugin:1.8:run (add-dll) on project log4jna-win32dll: An Ant BuildException has occured: The following error occurred while executing this line:
[ERROR] ...log4jna-win32dll\src\build.xml:132: Execute failed: java.io.IOException: Cannot run program "mc" (in directory "...log4jna-win32dll"): CreateProcess error=2, The system cannot find the file specified

```

workaround is by commenting the module reference (the resource dll is only needed at run time):
```xml
  <modules>
<!--
    <module>log4jna-win32dll</module> -->
    <module>log4jna-api</module>
    <module>log4jna-demo</module>
    <module>log4jna-assembly</module>
  </modules>

```

```cmd
cd log4jna
mvn -Dmaven.test.skip=true clean package install
cd ..
```
```cmd
java -cp target\log4jna_sample-4.0-SNAPSHOT.jar;target\lib\* example.log4jna_sample.App "test message to be appended with some other text"
```

will log to console:

```text
[FATAL] 2024-01-16 12:40:48.951 [main] log4jna_sample.App - test message to be appended with some other text
[ERROR] 2024-01-16 12:40:49.389 [main] log4jna_sample.App - test message to be appended with some other text
[WARN] 2024-01-16 12:40:49.389 [main] log4jna_sample.App - test message to be appended with some other text
[INFO] 2024-01-16 12:40:49.389 [main] log4jna_sample.App - test message to be appended with some other text
[DEBUG] 2024-01-16 12:40:49.389 [main] log4jna_sample.App - test message to be appended with some other text
[TRACE] 2024-01-16 12:40:49.389 [main] log4jna_sample.App - test message to be appended with some other text
```


but event log logs will show a different message:

```powershell
get-eventlog -logname log4jna_sample -newest 5 |format-list
```
```text
Index              : 244
EntryType          : Information
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                      - this is a test
Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                      - this is a test}
Source             : example.log4jna_sample
TimeGenerated      : 1/16/2024 12:40:49 PM
TimeWritten        : 1/16/2024 12:40:49 PM
UserName           :

Index              : 243
EntryType          : Warning
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                      - this is a test
Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                      - this is a test}
Source             : example.log4jna_sample
TimeGenerated      : 1/16/2024 12:40:49 PM
TimeWritten        : 1/16/2024 12:40:49 PM
UserName           :

Index              : 242
EntryType          : Error
InstanceId         : 4096
Message            : Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                      - this is a test
Category           : Trace
CategoryNumber     : 1
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text - this is a test}
Source             : example.log4jna_sample
TimeGenerated      : 1/16/2024 12:40:49 PM
TimeWritten        : 1/16/2024 12:40:49 PM
UserName           :

```
the message is modified by `Win32EventLogAppender.java`

### See Also



 
  * [blog covering eventlog logback](http://ykchee.blogspot.com/2012/09/logback-nt-event-log-appender.html) - code unavailable
  * [guide to Java Logging with Logback](https://betterstack.com/community/guides/logging/java/logback/)
  * https://logback.qos.ch/manual/appenders.html
  * [guide To Logback](https://www.baeldung.com/logback)
  * https://learn.microsoft.com/en-us/windows/win32/wes/identifying-the-provider?redirectedfrom=MSDN
  * https://stackoverflow.com/questions/12862697/difference-between-different-eventlogmessages-dll-in-net-framework-folders

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



