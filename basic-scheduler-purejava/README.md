### Info


This directory contains code of __pure Java Scheduler__
and its dependency classes

  * `JobScheduler` [java2s](http://www.java2s.com/Code/Java/Threads/JobScheduler.htm)
  * `DaemonLock` :   [java2s](http://www.java2s.com/Code/Java/Threads/DaemonLock.htm)
  * `ThreadPool` : [java2s](http://www.java2s.com/Code/Java/Threads/ThreadPool2.htm)
  * `BusyFlag`, `CondVar`: [java2s](http://www.java2s.com/Code/Java/Threads/BusyFlag.htm)


### Usage
```sh
mvn test
mvn clean -Dmaven.test.skip=true package
```
```sh
java -jar target/scheduler.jar
```
* set the size of `CircularList` to 10 to see the index rollover quickly:
```text

 # 1\System\Processor Queue Length \System\Processor Queue Length 1st=3 2nd=0 multi=1
 # 2\System\Processor Queue Length \System\Processor Queue Length 1st=4 2nd=0 multi=1
 # 3\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 4\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 5\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 6\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 7\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 8\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 9\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 0\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 1\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 2\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 3\System\Processor Queue Length \System\Processor Queue Length 1st=13 2nd=0 multi=1
 # 4\System\Processor Queue Length \System\Processor Queue Length 1st=50 2nd=0 multi=1
 # 5\System\Processor Queue Length \System\Processor Queue Length 1st=33 2nd=0 multi=1
 # 6\System\Processor Queue Length \System\Processor Queue Length 1st=21 2nd=0 multi=1
 # 7\System\Processor Queue Length \System\Processor Queue Length 1st=24 2nd=0 multi=1
 # 8\System\Processor Queue Length \System\Processor Queue Length 1st=20 2nd=0 multi=1
 # 9\System\Processor Queue Length \System\Processor Queue Length 1st=31 2nd=0 multi=1
 # 0\System\Processor Queue Length \System\Processor Queue Length 1st=26 2nd=0 multi=1
 # 1\System\Processor Queue Length \System\Processor Queue Length 1st=25 2nd=0 multi=1
```

Run two schedules to collect values and compute average (in verbose mode):
```java
JobScheduler jobScheduler = new JobScheduler(0);

PerformanceCounterTask collectorTask = new PerformanceCounterTask();
collectorTask.setTask(MessageType.COLLECT);
collectorTask.setVerbose(false);
jobScheduler.executeInAndRepeat(collectorTask, interval, JobScheduler.PER_SECOND);

PerformanceCounterTask computeTask = new PerformanceCounterTask();
computeTask.setVerbose(true);
computeTask.setDebug(true);
computeTask.setTask(MessageType.COMPUTE);
jobScheduler.executeInAndRepeat(computeTask, interval * 10, 30 * JobScheduler.PER_SECOND);

```
```text
...
 # 68\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 69\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
averaging of 69 values
04/10/2022 01:11:30.863 2
04/10/2022 01:11:31.521 0
04/10/2022 01:11:32.509 0
04/10/2022 01:11:33.509 0
04/10/2022 01:11:34.511 0
04/10/2022 01:11:35.519 0
04/10/2022 01:11:36.509 1
04/10/2022 01:11:37.509 0
04/10/2022 01:11:38.538 24
04/10/2022 01:11:39.527 65
04/10/2022 01:11:40.606 72
04/10/2022 01:11:41.507 0
04/10/2022 01:11:42.510 0
04/10/2022 01:11:43.509 0
04/10/2022 01:11:44.510 0
04/10/2022 01:11:45.510 0
04/10/2022 01:11:46.519 5
04/10/2022 01:11:47.588 19
04/10/2022 01:11:48.510 25
04/10/2022 01:11:49.508 0
04/10/2022 01:11:50.506 0
04/10/2022 01:11:51.507 0
04/10/2022 01:11:52.509 0
04/10/2022 01:11:53.509 0
04/10/2022 01:11:54.507 0
04/10/2022 01:11:55.534 1
04/10/2022 01:11:56.509 0
04/10/2022 01:11:57.514 0
04/10/2022 01:11:58.515 10
04/10/2022 01:11:59.508 0
04/10/2022 01:12:00.563 0
04/10/2022 01:12:01.508 0
04/10/2022 01:12:02.518 7
04/10/2022 01:12:03.508 0
04/10/2022 01:12:04.508 0
04/10/2022 01:12:05.507 0
04/10/2022 01:12:06.505 0
04/10/2022 01:12:07.508 0
04/10/2022 01:12:08.509 1
04/10/2022 01:12:09.506 0
04/10/2022 01:12:10.508 2
04/10/2022 01:12:11.506 0
04/10/2022 01:12:12.509 0
04/10/2022 01:12:13.508 0
04/10/2022 01:12:14.508 0
04/10/2022 01:12:15.507 0
04/10/2022 01:12:16.508 0
04/10/2022 01:12:17.509 4
04/10/2022 01:12:18.512 0
04/10/2022 01:12:19.507 0
04/10/2022 01:12:20.506 0
04/10/2022 01:12:21.509 0
04/10/2022 01:12:22.515 0
04/10/2022 01:12:23.509 0
04/10/2022 01:12:24.509 0
04/10/2022 01:12:25.525 26
04/10/2022 01:12:26.527 37
04/10/2022 01:12:27.506 0
04/10/2022 01:12:28.506 0
04/10/2022 01:12:29.508 0
5.016667 (60/69)
 # 70\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 71\System\Processor Queue Length \System\Processor Queue Length 1st=13 2nd=0 multi=1
 # 72\System\Processor Queue Length \System\Processor Queue Length 1st=8 2nd=0 multi=1
 # 73\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 74\System\Processor Queue Length \System\Processor Queue Length 1st=5 2nd=0 multi=1
 # 75\System\Processor Queue Length \System\Processor Queue Length 1st=8 2nd=0 multi=1
 # 76\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 77\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 78\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 79\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 80\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 81\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 82\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 83\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 84\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 85\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 86\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 87\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 88\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 89\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 90\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 91\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 92\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 93\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 94\System\Processor Queue Length \System\Processor Queue Length 1st=48 2nd=0 multi=1
 # 95\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 96\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 97\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 98\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
 # 99\System\Processor Queue Length \System\Processor Queue Length 1st=0 2nd=0 multi=1
averaging of 99 values
...
```
same will be shown if the class is run via classpath command
```cmd
java -cp target\scheduler.jar example.PerformanceCounterJobScheduler
```
```text
 # 0\System\Processor Queue Length Fri Mar 18 15:58:48 EDT 2022 1st=13 2nd=0  multi=1
 # 1\System\Processor Queue Length Fri Mar 18 15:58:50 EDT 2022 1st=50 2nd=0 multi=1
 # 2\System\Processor Queue Length Fri Mar 18 15:58:51 EDT 2022 1st=33 2nd=0 multi=1
 # 3\System\Processor Queue Length Fri Mar 18 15:58:52 EDT 2022 1st=21 2nd=0 multi=1
 # 4\System\Processor Queue Length Fri Mar 18 15:58:53 EDT 2022 1st=24 2nd=0 multi=1
 # 5\System\Processor Queue Length Fri Mar 18 15:58:54 EDT 2022 1st=20 2nd=0 multi=1
 # 6\System\Processor Queue Length Fri Mar 18 15:58:55 EDT 2022 1st=31 2nd=0 multi=1
 # 7\System\Processor Queue Length Fri Mar 18 15:58:56 EDT 2022 1st=26 2nd=0 multi=1
 # 8\System\Processor Queue Length Fri Mar 18 15:58:58 EDT 2022 1st=25 2nd=0 multi=1
 # 9\System\Processor Queue Length Fri Mar 18 15:58:58 EDT 2022 1st=25 2nd=0 multi=1
 # 0\Sys # 0tem\Processor Queue Length Fri Mar 18 15:58:59 EDT 2022 1st=23 2nd=0 multi=1
```
- the data is stores in a custom `CircularList` of size 10 which implements a `List<DataEntry>`
which allows using a factory `synchronizedList`
method of `Collections` class to make thread safe apppends. Note, the `size` method of the `CircularList` is used to
return the index the next element will be written to (only used for logging).

### Note:
occassionally see failing test:
```cmd
mvn test
```
```text
[ERROR] Tests run: 6, Failures: 1, Errors: 0, Skipped: 1, Time elapsed: 23.813 s
 <<< FAILURE! - in example.JobSchedulerTest
[ERROR] test1(example.JobSchedulerTest)  Time elapsed: 0.922 s  <<< FAILURE!
Wanted but not invoked:
runnable.run();
-> at example.JobSchedulerTest.test1(JobSchedulerTest.java:61)
Actually, there were zero interactions with this mock.
```
usually error disappears in a rerun
### See Also
   * https://www.baeldung.com/thread-pool-java-and-guava
   * https://www.baeldung.com/mockito-void-methods
   * https://stackoverflow.com/questions/32319640/how-to-test-spring-scheduled/49930983
   * https://javadoc.io/doc/org.mockito/mockito-core/latest/org/mockito/Mockito.html#4
   * https://www.baeldung.com/java-ring-buffer
   * https://www.geeksforgeeks.org/java-program-to-implement-circular-buffer/
   * https://stackoverflow.com/questions/44942493/multiple-aggregate-functions-in-java-8-stream-api
   * https://www.codejava.net/java-core/collections/java-stream-aggregate-functions-examples-intermediate-operations
   * https://docs.oracle.com/javase/tutorial/collections/streams/index.html
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
