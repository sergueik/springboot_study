### Info

replica of [yokra9/log4jna_samle](https://github.com/yokra9/log4jna_samle) with added missing jna dependency and other minor fixes.
The underlying [Log4jna] library of Java Native Windows Event Log appenders for [log4j]() is modified in particular to
enable configuring different event IDs for logged messages via `log4j.xml`
and to use the system-provided message dll `C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll` instead of the embedded `src\main\resources\Win32EventLogAppender.dll`
The creation of the custom event log can be done outside of the application, and logging does not require elevated (a.k.a admin) privileges

### Usage

* recycle and create empty log using elevated powershell prompt

```powershell
get-eventlog log4jna_sample
remove-eventlog -logname log4jna_sample
$resource_dll_path = '%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```

this will create 
```txt
c:\Windows\System32\winevt\Logs\log4jna_sample.evtx
```

![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-log4j-eventlog/screenshots/capture-eventlog-tree.png)

```cmd
cacls.exe c:\Windows\System32\winevt\Logs\log4jna_sample.evtx
```
```text
c:\Windows\System32\winevt\Logs\log4jna_sample.evtx NT SERVICE\eventlog:(ID)F
                                                    NT AUTHORITY\SYSTEM:(ID)F
                                                    BUILTIN\Administrators:(ID)F

```
* confirm with
```cmd
wevtutil.exe gl log4jna_sample
```
this will print
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
* also examine event sources:
```cmd
wevtutil.exe ep | where-object { $_ -match '.*log4jna_sample'
```
```text
example.log4jna_sample
log4jna_sample
log4jna_sample

```

* configure `log4j.xml`:
```XML
<Property name="dllfile">%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll</Property>
<Win32EventLog name="EventLog" messageId="3" eventMessageFile="${dllfile}" categoryMessageFile="${dllfile}" source="example.log4jna_sample" application="log4jna_sample">
```
temporarily add several EventLog appenders with distinct MessageId:
```
```

* run the tests

```cmd
mvn test
```
observe presence of specific ids in Evenet Log:

![Event log Config Before](https://github.com/sergueik/springboot_study/blob/master/basic-log4j-eventlog/screenshots/capture-different-ids.png)

* build app. If the custom event log was not created, you will need to be skipping tests in non-elevated command prompt:
```sh
mvn -Dmaven.test.skip=true clean package
```
* run the app
```
java -cp target\log4jna_sample-0.5.0-SNAPSHOT.jar;target\lib\* example.log4jna_sample.App "test message from Java app"
```
* will see operation logged to console

```text

[FATAL] 2024-05-02 14:54:45.246 [main] log4jna_sample.App - test message to be appended with some other text
Registry Key exists
Reporting event messageID: 3
[ERROR] 2024-05-02 14:54:46.339 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[WARN] 2024-05-02 14:54:46.339 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[INFO] 2024-05-02 14:54:46.355 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[DEBUG] 2024-05-02 14:54:46.355 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[TRACE] 2024-05-02 14:54:46.371 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3

```

![Event log Entry](https://github.com/sergueik/springboot_study/blob/master/basic-log4j-eventlog/screenshots/capture-eventlog-entry.png)


* examine the custom log:
```powershell
get-eventlog -logname log4jna_sample -newest 1| format-list
```

this will show
```text
Index              : 348
EntryType          : 0
InstanceId         : 3
Message            : Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text

Category           : %1
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/2/2024 2:59:46 PM
TimeWritten        : 5/2/2024 2:59:46 PM
UserName           :


```


### NOTE

* test must be run in elevated console. if this is not followed observe the following errors
```cmd
remove-eventlog -logname log4jna_sample
```
* run the app
```
java -cp target\log4jna_sample-0.5.0-SNAPSHOT.jar;target\lib\* example.log4jna_sample.App "test message to be appended with some other text"
```
* will see operation logged to console

```text

[FATAL] 2024-05-02 14:54:45.246 [main] log4jna_sample.App - test message to be appended with some other text
Registry Key exists
Reporting event messageID: 3
[ERROR] 2024-05-02 14:54:46.339 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[WARN] 2024-05-02 14:54:46.339 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[INFO] 2024-05-02 14:54:46.355 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[DEBUG] 2024-05-02 14:54:46.355 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3
[TRACE] 2024-05-02 14:54:46.371 [main] log4jna_sample.App - test message to be appended with some other text
Reporting event messageID: 3

```
* examine the custom log:
```powershell
get-eventlog -logname log4jna_sample -newest 1| format-list
```

this will show
```text
Index              : 348
EntryType          : 0
InstanceId         : 3
Message            : Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text

Category           : %1
CategoryNumber     : 6
ReplacementStrings : {Thread: main
                     Logger: log4jna_sample.App
                     Message: test message to be appended with some other text
                     }
Source             : example.log4jna_sample
TimeGenerated      : 5/2/2024 2:59:46 PM
TimeWritten        : 5/2/2024 2:59:46 PM
UserName           :


```


### NOTE

* test must be run in elevated console. if this is not followed observe the following errors
```cmd
remove-eventlog -logname log4jna_sample
```
```cmd
mvn clean test
```

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
### See Also


  * __Configuring different event IDs for logged messages__ [open issue](https://github.com/dblock/log4jna/issues/244)
  * [blog covering eventlog logback](http://ykchee.blogspot.com/2012/09/logback-nt-event-log-appender.html) - code unavailable
  * [guide to Java Logging with Logback](https://betterstack.com/community/guides/logging/java/logback/)
  * https://logback.qos.ch/manual/appenders.html
  * [guide To Logback](https://www.baeldung.com/logback)
  * https://learn.microsoft.com/en-us/windows/win32/wes/identifying-the-provider?redirectedfrom=MSDN
  * https://stackoverflow.com/questions/12862697/difference-between-different-eventlogmessages-dll-in-net-framework-folders

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



