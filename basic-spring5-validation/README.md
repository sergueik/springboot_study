### Info

this directory contains execrises from __Validating Data Input in Spring 5 Web Applications__ Pluralsight [Training](https://app.pluralsight.com/library/courses/spring-5-web-applications-validating-data-input/table-of-contents)
ilustrating `@RestControllerAdvice` annotation for Spring app global custom appliction exception handler development

### Note

* converted to Java __1.8__
* partially replaced the __Lombok__ annotations with vanilla IDE-generated code

### Usage

* run application
```sh
mvn spring-boot:run
```
* post invalid data

```sh
curl -X POST  -H "Content-Type: application/json" -d '{"firstName":"","middleName":"","lastName":"" ,"age":0,"email": ""}' http://localhost:8080/patients
```
the outcome is different on a Windows and Linux host

#### Windows Host
* observe internal server error
```json
{
  "timestamp": "2022-12-11T22:45:43.532+00:00",
  "status": 500,
  "error": "Internal Server Error",
  "message": "",
  "path": "/patients"
}
```
with exception logged to console
```text
2022-12-11 17:45:43.495 ERROR 6544 --- [nio-8080-exec-8] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception
[Request processing failed;
nested exception is java.lang.IllegalArgumentException: First name not provided] with root cause
java.lang.IllegalArgumentException: First name not provided
```

instead of body with a `ErrorResponse`
```json
{
  "message": "First name not provided"
}
```

confirmed with request
```sh
curl -X POST  -H "Content-Type: application/json" -d '{"firstName":"John","middleName":"","lastName":"" ,"age":0,"email": ""}' http://localhost:8080/patients
```
and console log
```
2022-12-11 17:53:11.490 ERROR 6544 --- [io-8080-exec-10] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context
with path [] threw exception [Request processing failed; nested exception is java.lang.IllegalArgumentException: Email is invalid]
with root cause java.lang.IllegalArgumentException: Email is invalid
```
Probably with Spring Boot 2.3.4 the annotations `@RestControllerAdvice("example")` `@ExceptionHandler(IllegalArgumentException.class)` `@ResponseStatus(INTERNAL_SERVER_ERROR)` does not work

#### Linux Host

With the same dependency versions, the call

```sh
curl -X POST  -H "Content-Type: application/json" -d '{"firstName":"","middleName":"","lastName":"" ,"age":0,"email": ""}' http://localhost:8080/patients
```

returns the custom error object in page body:
```json
{
  "message":"First name not provided"
}
```

and the call
```sh
curl -X POST  -H "Content-Type: application/json" -d '{"firstName":"John","middleName":"","lastName":"" ,"age":0,"email": ""}' http://localhost:8080/patients
```

is returning

```json
{"message":"Email is invalid"}
```

the environment is Java 1.8 on both hosts
```sh
java -version
```
```text
java version "1.8.0_161"
Java(TM) SE Runtime Environment (build 1.8.0_161-b12)
Java HotSpot(TM) 64-Bit Server VM (build 25.161-b12, mixed mode)
```
and
```cmd
java.exe -version
```
```text
java version "1.8.0_101"
Java(TM) SE Runtime Environment (build 1.8.0_101-b13)
Java HotSpot(TM) 64-Bit Server VM (build 25.101-b13, mixed mode)
```


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
