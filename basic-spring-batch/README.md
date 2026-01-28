### Info

This directory contains a replica of [spring Boot / Spring Batch Example](https://github.com/TechPrimers/spring-batch-example-1) repository illustrating running jobs synchronously. Added the method to check the job status (there is currently only one named job).

### Background

[Spring Batch](https://en.wikipedia.org/wiki/Spring_Batch) was created through a collaboration after Accenture contributed its own previously proprietary batch processing architecture frameworks, based on decades of experience, to the SpringSource open-source project. The Spring Boot was created and first released in 2014  therefore a Spring Batch tutorials often still cover XML configurations.

### Usage
* build and start application
```sh
mvn spring-boot:run
```
* access the REST endpoint for starting single Spring Batch job which will load a CSV to DB
 `http://localhost:8081/load`:
```sh
for CNT in $(seq 1 1 100); do curl -s -k http://localhost:8081/load; done
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
