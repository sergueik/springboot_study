### Info

Attempt to exercise old examples of unix signal handling in Java. Currently does not work.

### Usage

```sh
mvn package
java -Xrs -cp target/example-signal-handler-0.2.0-SNAPSHOT.jar example.App
```
it will print to console:
```text
CustomSignalHandler set to handle SYS (31)
Sleeping for 240 seconds: hit me with signals!
```
from another console, send `SIGUSR2` signal
```sh
ps ax | grep example.Ap[p] | awk '{print $1}' |xargs -IX kill -12 X
```
this will crash the java application with the stack trace written to the console:

```text
#
# A fatal error has been detected by the Java Runtime Environment:
#
#  SIGSEGV (0xb) at pc=0x00007f243fd1d82e, pid=17392, tid=0x00007f2441059b80
#
# JRE version: Java(TM) SE Runtime Environment (8.0_161-b12) (build 1.8.0_161-b12)
# Java VM: Java HotSpot(TM) 64-Bit Server VM (25.161-b12 mixed mode linux-amd64 compressed oops)
# Problematic frame:
# V  [libjvm.so+0x92982e]  SR_handler(int, siginfo*, ucontext*)+0x3e
#
# Failed to write core dump. Core dumps have been disabled. To enable core dumping, try "ulimit -c unlimited" before starting Java again
#
# An error report file with more information is saved as:
# /home/sergueik/src/springboot_study/basic-signal/hs_err_pid17392.log
#
# If you would like to submit a bug report, please visit:
#   http://bugreport.java.com/bugreport/crash.jsp
#
Aborted
```
using numeric signal `31` is not reaching the application
```sh
kill -31
```
using simbolic name `USR1` 
```sh
ps ax | grep example.Ap[p] | awk '{print $1}' |xargs -IX kill -USR1 X
```
works as expected: application prints confirmation message and quits:

```text
CustomSignalHandler set to handle SYS (31)
Sleeping for 240 seconds: hit me with signals!
```
```text
User defined signal 1
```
### See Also

  * https://ringlord.com/dl/Signals-in-Java.pdf
  * https://kostenko.org/blog/2019/08/java-catch-os-signals.html
  * https://www.ibm.com/docs/en/ztpf/2019?topic=signals-used-by-jvm
  * https://m.cafe.daum.net/sepro/5CO5/100?listURI=%2Fsepro%2F_rece
