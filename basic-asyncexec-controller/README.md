### Info

https://www.journaldev.com/20457/spring-async-annotation


### Usage

* run app
```sh
mvn spring-boot:run
```
 - the app starts few tasks
* cancel app via REST call
``sh
curl -X POST http://localhost:8080/cancel  -H 'Content-Type: application/json'
```
* the app will report being canceled:
```text
2022-01-31 19:31:40.626 ERROR 9164 --- [ main] o.s.boot.SpringApplication: Application run failed
java.lang.IllegalStateException: Failed to execute CommandLineRunner
at example.Application.main(Application.java:19) [classes/:na]
Caused by: java.util.concurrent.CancellationException: null
...
at java.util.concurrent.CompletableFuture.cancel
at example.runner.CustomApplicationRunner.cancel
at example.controller.Controller.cancel
```

### See Also

https://howtodoinjava.com/spring-boot2/rest/async-rest-controller-callable/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
