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
mvn -Dmaven.test.skip=true package
```
```sh
java.exe -cp target\jna_eventlog-0.1.0-SNAPSHOT.jar;target\lib\* example.App the quick brown fox jumps over the lazy dog
```

![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

* verify

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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



\