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
### See Also

   * [evaluate if the current user is a member the "well-known" sid Administator group](https://www.rgagnon.com/javadetails/java-detect-if-current-user-is-admin-using-jna.html) - not a UAC replacement
   * https://superuser.com/questions/809901/check-for-elevation-at-command-prompt
   * http://blogs.msdn.com/b/virtual_pc_guy/archive/2010/09/23/a-self-elevating-powershell-script.aspx
   * https://stackoverflow.com/questions/7985755/how-to-detect-if-cmd-is-running-as-administrator-has-elevated-privileges
   * https://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
