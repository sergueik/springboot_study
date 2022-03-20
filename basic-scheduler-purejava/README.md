### Info


This directory contains code of __pure Java Scheduler__
and its dependency classes

  * `JobScheduler` [java2s](http://www.java2s.com/Code/Java/Threads/JobScheduler.htm)
  * `DaemonLock` :   [java2s](http://www.java2s.com/Code/Java/Threads/DaemonLock.htm)
  * `ThreadPool` : [java2s](http://www.java2s.com/Code/Java/Threads/ThreadPool2.htm)
  * `BusyFlag`, `CondVar`: [java2s](http://www.java2s.com/Code/Java/Threads/BusyFlag.htm)


### Usage
```sh
mvn clean package

```
```
java -jar target/scheduler.jar
```
```text
\System\Processor Queue Length Fri Mar 18 15:46:48 EDT 2022 1st=3 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:49 EDT 2022 1st=4 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:50 EDT 2022 1st=0 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:51 EDT 2022 1st=0 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:52 EDT 2022 1st=0 2nd=0 multi=1 
\System\Processor Queue Length Fri Mar 18 15:46:53 EDT 2022 1st=0 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:54 EDT 2022 1st=0 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:55 EDT 2022 1st=0 2nd=0 multi=1 
\System\Processor Queue Length Fri Mar 18 15:46:56 EDT 2022 1st=0 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:46:57 EDT 2022 1st=0 2nd=0 multi=1
```


```cmd
java -cp target\scheduler.jar example.PerformanceCounterJobScheduler
```
```text
\System\Processor Queue Length Fri Mar 18 15:58:48 EDT 2022 1st=13 2nd=0  multi=1
\System\Processor Queue Length Fri Mar 18 15:58:50 EDT 2022 1st=50 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:51 EDT 2022 1st=33 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:52 EDT 2022 1st=21 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:53 EDT 2022 1st=24 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:54 EDT 2022 1st=20 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:55 EDT 2022 1st=31 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:56 EDT 2022 1st=26 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:58 EDT 2022 1st=25 2nd=0 multi=1
\System\Processor Queue Length Fri Mar 18 15:58:59 EDT 2022 1st=23 2nd=0 multi=1
```
### See Also
https://www.baeldung.com/thread-pool-java-and-guava

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
