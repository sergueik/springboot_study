### Info
   this directory contains code examples based on [testing the @Scheduled Annotation](https://www.baeldung.com/spring-testing-scheduled-annotation)
- illustrating passing test of enabled scheduled methods

### Usage

* run the test
```sh
mvn test
```
the test will pass with the Schedule settings read from `application.properties`
* return overriging the `example.rate` on the commandline

```sh
mvn clean -Dexample.rate=1000 test
```
the test will log
```text
...
17:45:52.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 1 time
17:45:53.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 2 times
17:45:54.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 3 times
17:45:55.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 4 times
17:45:56.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 5 times
17:45:57.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 6 times
17:45:58.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 7 times
17:45:59.033 [scheduling-1] INFO example.tasks.ScheduledTask - executing 8 times
...
```
the test will now fail because it expects certain number of execitions and with a reduced rate it will become too many:
```text
[ERROR] Failures:
[ERROR]   ScheduledTaskTest.test:29
Expected: a value less than <4>
     but: <8> was greater than <4>
```
```sh
mvn clean -Dexample.rate=120000 test
```
```text
[ERROR] Failures:
[ERROR]   ScheduledTaskTest.test:28
Expected: a value greater than <2>
     but: <1> was less than <2>
```
NOTE: tuning the repetition expectations is somewhat labor intensive.
NOTE: making the fluent test fail wat not achieved.

### See Also

  * https://stackoverflow.com/questions/40684903/disable-schedule-on-spring-boot-integrationtest/52260620
  * https://stackoverflow.com/questions/29014496/disable-enablescheduling-on-spring-tests
  * scheduling a job with Spring programmatically (with fixedRate set dynamically) [stackoverflow discussion](https://stackoverflow.com/questions/14630539/scheduling-a-job-with-spring-programmatically-with-fixedrate-set-dynamically)
  * [enabling conditionally](https://www.baeldung.com/spring-scheduled-enabled-conditionally) 
  * [programmatically Scheduling and Cancelling Task with Spring Boot](https://dev.to/fluox/programmatically-scheduling-and-cancelling-task-with-spring-boot-2b2)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
