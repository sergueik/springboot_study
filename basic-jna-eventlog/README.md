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

![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

* verify

```powershell
get-eventlog -logname log4jna_sample -newest 1 |format-list
```
or  
```cmd
wevtutil.exe qe   log4jna_sample /rd:true /f:text /c:1
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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



\
