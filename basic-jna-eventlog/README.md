### Info

Standalone Event log logger derived from [dblock/log4jna](https://github.com/dblock/log4jna) - removed the dependency on Log4j with the plan to use with Logback


### Usage

* clear the custom Event Log `log4jna_sample`
```cmd
wevtutil.exe clear-log log4jna_sample
```
* run from elevated console. ([WIP] subsequent runs will work for any user)
to create the event source `log4jna_sample` in `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx`
```sh
mvn clean package
```
```sh
java -jar target\example.jna_eventlog.jar the quick brown fox jumps over the lazy dog
```
* review Event Log entries

![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

* query in console

```powershell
get-eventlog -logname log4jna_sample -newest 1 |format-list
```
reveals
```text
Index              : 26
EntryType          : Information
InstanceId         : 4096
Message            : the quick brown fox jumps over the lazy dog  - this is a test
Category           : Info
CategoryNumber     : 3
ReplacementStrings : {the quick brown fox jumps over the lazy dog  - this is a  test}
Source             : example.log4jna_sample
TimeGenerated      : 25.01.2024 8:07:00
TimeWritten        : 25.01.2024 8:07:00
UserName           :
```

alternatively
```cmd
wevtutil.exe qe log4jna_sample /rd:true /f:text /c:1
```
```text
Event[0]:
  Log Name: log4jna_sample
  Source: example.log4jna_sample
  Date: 2024-01-25T18:02:54.0220000Z
  Event ID: 4096
  Task: Info
  Level: Сведения
  Opcode: Сведения
  Keyword: Классический
  User: N/A
  User Name: N/A
  Computer: DESKTOP-82V9KDO
  Description:
the quick brown fox jumps over the lazy dog  - this is a test

```
### TODO

the Windows Event Log messages can be addded by a regular user. However the creation of the application-specicic Windows Event Log with all its categories an resources requires elevation. There is currently no working Java code capable of perfoeming the UAC check - the method `checkCurrentUserIsAdmin`
relies on checking th membership of the opetating user in the `BUILTIN\Administrators` group and reports false positive.
#### Demo

* try to do logging as a regular user when no event log exists
```cmd	
mvn clean package
java -jar target\example.jna_eventlog.jar message
```
see the technical info
```text
registerEventSource
Check if CurrentUser is Admin
group: None
group: Everyone
group: Local account and member of Administrators group
group: HelpLibraryUpdaters
group: HomeUsers
group: Administrators
Current User Is Admin
```

and the exception
```text
Exception in thread "main" java.lang.RuntimeException: Could not register event source: Access is denied.
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:126)
        at example.Win32EventLogAppender.append(Win32EventLogAppender.java:136)
```

### Using System Event Log Resources

  *  remove  the custom event log created by Java by running system cmdlets
  * remove-eventlog -LogName 'log4jna_sample' 
    -inspect through Registry but not modify directly
  *  create the same custom event log  specifying the categorymessagefile and eventmessagefile to be
```powershell
$resource_dll_path = (resolve-path 'src\main\resources\Win32EventLogAppender.dll').Path
$resource_dll_path =  'C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
$resource_dll_path = '%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```

  * try both literal path and expand expression

  * update the `jna_eventlog` Java source arguments to make it confirm it has the log available


run as regular user:
```cmd
mvn clean package
```
```cmd
java -jar target\example.jna_eventlog.jar the quick brown fox jumps over the lazy dome
```
this will successfully log the message:
```text
RegisterEventSource: EventMessageFile: "%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll", CategoryMessageFile: "%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll"
```
it will be found

```powershell
get-eventlog -logname 'log4jna_sample'
```
```text
   Index Time          EntryType   Source                 InstanceID Message
   ----- ----          ---------   ------                 ---------- -------
      39 Apr 11 12:55  Information example.log4jna_s...         4096 the qui...
```


```powershell
 get-eventlog -logname log4jna_sample -newest 1 |format-list
```


```text

Index              : 39
EntryType          : Information
InstanceId         : 4096
Message            : the quick brown fox jumps over the lazy dome
Category           : %1
CategoryNumber     : 3
ReplacementStrings : {the quick brown fox jumps over the lazy dome }
Source             : example.log4jna_sample
TimeGenerated      : 4/11/2024 12:55:10 PM
TimeWritten        : 4/11/2024 12:55:10 PM
UserName           :
```
NOTE: the older version will throw excdption:
```text
RegisterEventSource: EventMessageFile: "", CategoryMessageFile: ""

Check if CurrentUser is Admin
group: None 
group: Everyone
group: Local account and member of Administrators group
group: HelpLibraryUpdaters
group: HomeUsers
group: Administrators
Current User Is Admin
Exception in thread "main" java.lang.RuntimeException: Could not register event
source: Access is denied.
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:136)
        at example.Win32EventLogAppender.append(Win32EventLogAppender.java:146)
        at example.Win32EventLogAppender.append(Win32EventLogAppender.java:167)
        at example.App.main(App.java:29)
Caused by: com.sun.jna.platform.win32.Win32Exception: Access is denied.
        at com.sun.jna.platform.win32.Advapi32Util.registrySetStringValue(Advapi32Util.java:1557)
        at com.sun.jna.platform.win32.Advapi32Util.registrySetStringValue(Advapi32Util.java:1533)
        at example.Win32EventLogAppender.setVariableKeys(Win32EventLogAppender.java:230)
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:182)
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:131)
        ... 3 more

```


### See Also

   * [evaluate if the current user is a member the "well-known" sid Administator group](https://www.rgagnon.com/javadetails/java-detect-if-current-user-is-admin-using-jna.html) - not a UAC replacement
   * https://superuser.com/questions/809901/check-for-elevation-at-command-prompt
   * http://blogs.msdn.com/b/virtual_pc_guy/archive/2010/09/23/a-self-elevating-powershell-script.aspx
   * https://stackoverflow.com/questions/7985755/how-to-detect-if-cmd-is-running-as-administrator-has-elevated-privileges
   * https://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
