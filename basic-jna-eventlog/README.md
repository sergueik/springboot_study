### Info

Standalone Event log logger based on [dblock/log4jna](https://github.com/dblock/log4jna)

### Usage

* clear the log
```cmd
wevtutil.exe clear-log log4jna_sample
```
* run from elevated console. ([WIP] subsequent runs will work for any user)
to create the event source `log4jna_sample` in  `%SystemRoot%\System32\Winevt\Logs\log4jna_sample.evtx`
```sh
mvn clean package
```
```sh
java -jar target\example.jna_eventlog.jar the quick brown fox jumps over the lazy dog
```

this will print to console:
```text

04:26:37.314 [main] WARN  mapAppender - Event log from App message the quick brown fox jumps over the lazy dog
DEBUG: appending event message: 04:26:37.314 [main] WARN  mapAppender - Event log from App message the quick brown fox jumps over the lazy dog
```
and add Windows Event Log:
![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

* verify

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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



\