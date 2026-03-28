### Usage

```cmd
mvn package
```
```cmd
java -cp target\example.metered-digest.jar;target\lib\* example.Runner -inputfile README.md
```
```text
19:52:16.222 [main] INFO example.utils.Reader -- Digest input: README.md
9EE785D13131EE30C8E013F40DD5B375C42CA5D04E4A0359304DC062A9AA23A0
19:52:16.236 [main] INFO example.utils.Reader -- Processed 242 bytes in 8 ms
```
```
dir %USERPROFILE%\.docker\machine\machines\default\disk.vmdk
```
```
03/26/2026  07:59 PM     4,291,887,104 disk.vmdk
```
> Alternatively pick VM image, must be multi GB
> NOTE:  Make sure Docker/VM is not running
```cmd
set INPUTFILE=%USERPROFILE%\.docker\machine\machines\default\disk.vmdk
set INPUTFILE=%USERPROFILE%\Downloads\tiny10_21H2_x64_beta_1.iso

java -cp target\example.metered-digest.jar;target\lib\* example.Runner -inputfile %INPUTFILE%
```

```text
3BC8FC19205395488E6AB1BC9DEDABBED52E6B3D5D411EBEEDD80E8A8CBA5596
19:58:16.057 [main] INFO example.utils.Reader -- Processed 2564423680 bytes in 2579 ms
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
