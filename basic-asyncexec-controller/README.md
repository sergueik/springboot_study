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
or if `curl.exe` is not available, custom Powershell tweaked `System.Net.WebRequest` call:
```powershell
.\cancel.ps1 -url http://localhost:8080/cancel -debug
```
the script will print the status:
```text
POST to http://localhost:8080/cancel
Response status code: 200/OK
Reading response
Response:
OK
```
or, in case of a failure
```text
POST to http://localhost:8080/cancel
Exception:
Status: ConnectFailure
StatusCode:
Message: Unable to connect to the remote server
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

  * https://howtodoinjava.com/spring-boot2/rest/async-rest-controller-callable/
  * http://www.avajava.com/tutorials/lessons/how-do-i-run-another-application-from-java.html?page=2
  * https://www.baeldung.com/java-create-jar

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
