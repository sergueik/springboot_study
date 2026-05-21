### Info
   this directory contains code examples based on [testing the @Scheduled Annotation](https://www.baeldung.com/spring-testing-scheduled-annotation)
- illustrating passing test of enabled scheduled methods
### Background

Exactly one of the `cron()`, `fixedDelay()`, or `fixedRate()` attributes of `Scheduled` interface must be specified.



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

2026-05-21 16:33:02.953  INFO 31008 --- [           main] example.tasks.CronScheduledTaskTest      : No active profile set, falling back to 1 default profile: "default"
2026-05-21 16:33:03.208  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 0 times
2026-05-21 16:33:03.208  INFO 31008 --- [           main] example.tasks.CronScheduledTaskTest      : Started CronScheduledTaskTest in 0.523 seconds (JVM running for 9.625)
2026-05-21 16:33:04.002  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395584002 0 times
2026-05-21 16:33:04.174  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 3 times
2026-05-21 16:33:05.000  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395585000 1 time
2026-05-21 16:33:06.000  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395586000 2 times
2026-05-21 16:33:06.209  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 1 time
2026-05-21 16:33:07.003  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395587003 3 times
2026-05-21 16:33:07.174  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 4 times
2026-05-21 16:33:08.003  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395588003 4 times
[INFO] Tests run: 2, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 5.428 s - in example.tasks.CronScheduledTaskTest


org.springframework.boot.test.autoconfigure.webservices.client.MockWebServiceServerTestExecutionListener@629984eb]
2026-05-21 16:33:08.270  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 0 times
2026-05-21 16:33:09.002  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395589002 5 times
2026-05-21 16:33:09.208  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 2 times
2026-05-21 16:33:10.006  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395590006 6 times
2026-05-21 16:33:10.174  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 5 times
2026-05-21 16:33:11.002  INFO 31008 --- [   scheduling-1] example.tasks.CronScheduledTask          : executed at 1779395591002 7 times
2026-05-21 16:33:11.260  INFO 31008 --- [   scheduling-1] example.tasks.FixedRateScheduledTask     : executing 1 time
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 3.255 s - in example.tasks.FixedRateScheduledTaskTest
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

  * https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/scheduling/annotation/Scheduled.html
  * https://stackoverflow.com/questions/40684903/disable-schedule-on-spring-boot-integrationtest/52260620
  * https://stackoverflow.com/questions/29014496/disable-enablescheduling-on-spring-tests
  * scheduling a job with Spring programmatically (with fixedRate set dynamically) [stackoverflow discussion](https://stackoverflow.com/questions/14630539/scheduling-a-job-with-spring-programmatically-with-fixedrate-set-dynamically)
  * [enabling conditionally](https://www.baeldung.com/spring-scheduled-enabled-conditionally) 
  * [programmatically Scheduling and Cancelling Task with Spring Boot](https://dev.to/fluox/programmatically-scheduling-and-cancelling-task-with-spring-boot-2b2)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
