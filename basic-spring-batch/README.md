### Info

This directory contains a replica of [spring Boot / Spring Batch Example](https://github.com/TechPrimers/spring-batch-example-1) repository illustrating running jobs synchronously. Added the nethod to check the job status (thre is only one named job)

### Usage
* build and start application
```sh
mvn  spring-boot:run
```
* access
* endpoint for starting single Spring Batch job which will load a CSV to DB
 `http://localhost:8081/load`:
```sh
for  cnt in $(seq 1 1 100); do curl -s -k http://localhost:8081/load;done
```
* browse the status page:
```sh
http://localhost:8081/status
```

- `http://localhost:8081/h2-console` - H2 Console for querying the in-memory tables.

* Check H2 Config
- `testdb` - Database.
- `sa` - User
- `password` - Password.

### See Also
  * stackoverflow [discussion](https://stackoverflow.com/questions/51085410/spring-batch-job-execution-status-in-response-body) of finding the started job info
  * [introduction to Spring Batch](https://www.baeldung.com/introduction-to-spring-batch) - a little heavy
  * https://www.programcreek.com/java-api-examples/?api=org.springframework.batch.core.explore.JobExplorer
