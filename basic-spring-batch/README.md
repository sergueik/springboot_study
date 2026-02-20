### Info

This directory contains a replica of [spring Boot / Spring Batch Example](https://github.com/TechPrimers/spring-batch-example-1) repository illustrating running jobs synchronously. Added the method to check the job status (there is currently only one named job).

### Background

[Spring Batch](https://en.wikipedia.org/wiki/Spring_Batch) was created through a collaboration after Accenture contributed its own previously proprietary batch processing architecture frameworks, based on decades of experience, to the SpringSource open-source project. The Spring Boot was created and first released in 2014  therefore a Spring Batch tutorials often still cover XML configurations.

### Usage
* build and start application
```sh
mvn spring-boot:run
```
* access the REST endpoint for starting single Spring Batch job which will load a CSV to DB `http://localhost:8081/load`:

```sh
curl http://localhost:8081/load
```
this will print to console
```text
"COMPLETED"
```
the applicaion console will show
```text
2026-02-18 23:36:43.911  INFO 8924 --- [nio-8081-exec-1] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=ETL-Load]] launched with the following parameters: [{time=1771475803874}]
2026-02-18 23:36:43.929  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job is starting. Job name: ETL-Load
2026-02-18 23:36:43.937  INFO 8924 --- [nio-8081-exec-1] o.s.batch.core.job.SimpleStepHandler     : Executing step: [ETL-file-load]
Converted from [001] to [Technology]
Converted from [002] to [Operations]
Converted from [003] to [Accounts]
Converted from [001] to [Technology]
Converted from [001] to [Technology]
Data Saved for Users: [User{id=1, name='Peter', dept='Technology', salary=12000}, User{id=2, name='Sam', dept='Operations', salary=13000}, User{id=3, name='Ryan', dept='Accounts', salary=10000}, User{id=4, name='Tex', dept='Technology', salary=20000}, User{id=5, name='Luna', dept='Technology', salary=40000}]
2026-02-18 23:36:49.042  INFO 8924 --- [nio-8081-exec-1] o.s.batch.core.step.AbstractStep         : Step: [ETL-file-load] executed in 5s105ms
2026-02-18 23:36:49.046  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job finished. Job name: ETL-Load
2026-02-18 23:36:49.047  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Status: COMPLETED
2026-02-18 23:36:49.047  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job completed successfully!
2026-02-18 23:36:49.048  INFO 8924 --- [nio-8081-exec-1] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=ETL-Load]] completed with the following parameters: [{time=1771475803874}] and the following status: [COMPLETED] in 5s124ms
2026-02-18 23:36:49.049  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Job name: ETL-load
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Last Job Instance is null
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : JobExecution: COMPLETED
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Batch ETL-Load is launched
2026-02-18 23:36:52.488  INFO 8924 --- [extShutdownHook] j.LocalContainerEntityManagerFactoryBean : Closing JPA EntityManagerFactory for persistence unit 'defaul
```
alternatively
```sh
for CNT in $(seq 1 1 100); do curl -s -k http://localhost:8081/load; done
```
* browse the status page:
```sh
http://localhost:8081/status
```
e.g.
```sh
curl -s http://localhost:8081/status
```
```text
"UNKNOWN"
```

- `http://localhost:8081/h2-console` - H2 Console for querying the in-memory tables.

* Check H2 Config
- `testdb` - Database.
- `sa` - User
- `password` - Password.

### Out  Of the Box Observability

How to solve the following complex task:
need a job-scope timeline inventory of “big scheme of things” method calls (reader/processor/writer, steps, job) — essentially a structured performance trace.

Vintage Spring Batch Frtamework offers some help out of the box!

#### Canonical Ways (from most idiomatic to most custom)


First of all one may simply use Spring Batch’s built-in execution model - already provide timings at no extra expense  at:

* Job level → JobExecution
* Step level → StepExecution

Each of these responds to:

`getStartTime()`
`getEndTime()`
`getLastUpdated()`
`getReadCount()`
`getWriteCount()`
`getCommitCount()`
`getRollbackCount()`


Example in one's implementation of `JobExecutionListener`:
```java
  @Override
  public void afterJob(JobExecution jobExecution) {

    for (StepExecution step : jobExecution.getStepExecutions()) {
      long durationMs =
        step.getEndTime().getTime() - step.getStartTime().getTime();
    
      logger.info("Step {} took {} ms (read={}, write={})",
        step.getStepName(),
        durationMs,
        step.getReadCount(),
        step.getWriteCount());
    }
  }
```

This demonstrates the canonical job-scope timing inventory in Spring Batch.

No custom context passing needed.

If a fine level `Chunk` / `Item` granularity is desired: use the `ItemRead`/`Process`/`Write` listeners...
for  method-level scope timing the following available:

  * `ItemReadListener`
  * `ItemProcessListener`
  * `ItemWriteListener`
  * `ChunkListener`

Example pattern:
```java
@Component
public class TimingItemListener implements ItemReadListener<User>, ItemProcessListener<User,User>, ItemWriteListener<User> {

  private long start;

  @Override
  public void beforeRead() {
    start = System.nanoTime();
  }

  @Override
  public void afterRead(User item) {
    long duration = System.nanoTime() - start;
    metrics.addReadTime(duration);
  }
}
```

Then summarize in the `afterJob()`.

Again, This is the standard __Spring Batch__ extension point for granular method timing.

Implement `JobExecutionListener` only aggregator logic, not the collector logic

Your intuition is right:
The timing inventory data travels from the individual methods through context and one may do the math in the `job event` overrides

Two canonical storage options:

Option A: in-memory bean (simplest)
```java
@Component
public class TimingRegistry {
    AtomicLong totalReadTime = new AtomicLong();
    AtomicLong totalProcessTime = new AtomicLong();
}
```

Injected into both listeners and job listener.

Option B: ExecutionContext (persisted & restartable)
`stepExecution.getExecutionContext().putLong("readTime", totalReadTime)`;


Then in `afterJob`:
```java
jobExecution.getStepExecutions().forEach(step -> {
    long readTime = step.getExecutionContext().getLong("readTime", 0L);
});

```
This is canonical when one aks for:

* restartability
* metrics persisted in DB



NOTE: If one wants "real Dom Perignon grade" observability: the canonical way is heavy duty: Micrometer + Spring Batch

Timers will be available for:

* job execution
* step execution
* item read/process/write

Export to:

* Prometheus
* Grafana
* JMX
* logs

The Minimal canonical architecture flow:
```text
ItemRead/Process/Write Listener
        ↓
   accumulate metrics
        ↓
JobExecutionListener.afterJob()
        ↓
   log / export / persist timeline
```
### Alternative Take
### Info

This directory contains a replica of [spring Boot / Spring Batch Example](https://github.com/TechPrimers/spring-batch-example-1) repository illustrating running jobs synchronously. Added the method to check the job status (there is currently only one named job).

### Background

[Spring Batch](https://en.wikipedia.org/wiki/Spring_Batch) was created through a collaboration after Accenture contributed its own previously proprietary batch processing architecture frameworks, based on decades of experience, to the SpringSource open-source project. The Spring Boot was created and first released in 2014  therefore a Spring Batch tutorials often still cover XML configurations.

### Usage
* build and start application
```sh
mvn spring-boot:run
```
* access the REST endpoint for starting single Spring Batch job which will load a CSV to DB `http://localhost:8081/load`:

```sh
curl http://localhost:8081/load
```
this will print to console
```text
"COMPLETED"
```
the applicaion console will show
```text
2026-02-18 23:36:43.911  INFO 8924 --- [nio-8081-exec-1] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=ETL-Load]] launched with the following parameters: [{time=1771475803874}]
2026-02-18 23:36:43.929  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job is starting. Job name: ETL-Load
2026-02-18 23:36:43.937  INFO 8924 --- [nio-8081-exec-1] o.s.batch.core.job.SimpleStepHandler     : Executing step: [ETL-file-load]
Converted from [001] to [Technology]
Converted from [002] to [Operations]
Converted from [003] to [Accounts]
Converted from [001] to [Technology]
Converted from [001] to [Technology]
Data Saved for Users: [User{id=1, name='Peter', dept='Technology', salary=12000}, User{id=2, name='Sam', dept='Operations', salary=13000}, User{id=3, name='Ryan', dept='Accounts', salary=10000}, User{id=4, name='Tex', dept='Technology', salary=20000}, User{id=5, name='Luna', dept='Technology', salary=40000}]
2026-02-18 23:36:49.042  INFO 8924 --- [nio-8081-exec-1] o.s.batch.core.step.AbstractStep         : Step: [ETL-file-load] executed in 5s105ms
2026-02-18 23:36:49.046  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job finished. Job name: ETL-Load
2026-02-18 23:36:49.047  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Status: COMPLETED
2026-02-18 23:36:49.047  INFO 8924 --- [nio-8081-exec-1] example.listener.Listener                : Job completed successfully!
2026-02-18 23:36:49.048  INFO 8924 --- [nio-8081-exec-1] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=ETL-Load]] completed with the following parameters: [{time=1771475803874}] and the following status: [COMPLETED] in 5s124ms
2026-02-18 23:36:49.049  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Job name: ETL-load
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Last Job Instance is null
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : JobExecution: COMPLETED
2026-02-18 23:36:49.050  INFO 8924 --- [nio-8081-exec-1] example.controller.LoadController        : Batch ETL-Load is launched
2026-02-18 23:36:52.488  INFO 8924 --- [extShutdownHook] j.LocalContainerEntityManagerFactoryBean : Closing JPA EntityManagerFactory for persistence unit 'defaul
```
alternatively
```sh
for CNT in $(seq 1 1 100); do curl -s -k http://localhost:8081/load; done
```
* browse the status page:
```sh
http://localhost:8081/status
```
e.g.
```sh
curl -s http://localhost:8081/status
```
```text
"UNKNOWN"
```

- `http://localhost:8081/h2-console` - H2 Console for querying the in-memory tables.

* Check H2 Config
- `testdb` - Database.
- `sa` - User
- `password` - Password.

### Out-of-the-Box Observability in Spring Batch

#### Goal

Collect a **job-scope timeline inventory** of “big picture” method calls
(reader / processor / writer / steps / job) — a structured performance trace.

Spring Batch already provides most of this **out of the box**.

---

#### 1. Built-in Timing (Canonical & Idiomatic)

Spring Batch automatically records timing at:

- **Job level → `JobExecution`**
- **Step level → `StepExecution`**

Available metrics:

- `getStartTime()`
- `getEndTime()`
- `getLastUpdated()`
- `getReadCount()`
- `getWriteCount()`
- `getCommitCount()`
- `getRollbackCount()`

##### Example (`JobExecutionListener.afterJob`)

```java
@Override
public void afterJob(JobExecution jobExecution) {

    for (StepExecution step : jobExecution.getStepExecutions()) {
        long durationMs =
            step.getEndTime().getTime() - step.getStartTime().getTime();

        logger.info("Step {} took {} ms (read={}, write={})",
            step.getStepName(),
            durationMs,
            step.getReadCount(),
            step.getWriteCount());
    }
}
```
### Spring  Batch Job Internals

![Login](https://github.com/sergueik/springboot_study/blob/master/basic-spring-batch/screenshots/capture-login.png)

![Console](https://github.com/sergueik/springboot_study/blob/master/basic-spring-batch/screenshots/capture-console.png)

```sql
SELECT STEP_NAME, READ_COUNT, WRITE_COUNT, STATUS, START_TIME, END_TIME
FROM BATCH_STEP_EXECUTION;
```

### See Also

  * stackoverflow [discussion](https://stackoverflow.com/questions/51085410/spring-batch-job-execution-status-in-response-body) of finding the started job info
  * [Spring Boot With Spring Batch](https://www.baeldung.com/spring-boot-spring-batch)
  * [introduction to Spring Batch](https://www.baeldung.com/introduction-to-spring-batch) - a little heavy
  * https://www.programcreek.com/java-api-examples/?api=org.springframework.batch.core.explore.JobExplorer
  * [misc. spring batch related code base](https://github.com/javacodingskills/SpringBatch)
  * [spring btch introduction](https://docs.spring.io/spring-batch/docs/2.2.x/reference/html/spring-batch-intro.html)
  * [Spring Batch Blog Series](https://keyholesoftware.com/introducing-spring-batch/)
  * [code samples of the Spring Batch in Action book](https://github.com/acogoluegnes/Spring-Batch-in-Action) published by Arnaud Cogoluegnes (one of the authors)
  * https://github.com/jojoldu/spring-batch-in-action
  * [testing](https://github.com/pkainulainen/spring-batch-examples)
  * https://github.com/shbaek/springbatchinaction
  * https://github.com/Apress/def-guide-spring-batch
  * https://github.com/Java-Techie-jt/spring-batch-example
  * [Enterprise-ready production-ready batch applications powered by Spring Boot](https://github.com/codecentric/spring-boot-starter-batch-web)
  * [spring Batch Tutorials](https://codenotfound.com/spring-batch-tutorials)
  * https://github.com/code-not-found/spring-batch

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
