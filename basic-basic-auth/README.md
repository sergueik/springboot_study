### Info

This directory contains code from [Spring boot security rest basic authentication example](https://howtodoinjava.com/spring-boot2/security-rest-basic-auth-example/) with tests temporarily disabled (the test needs SpringBoot parent version 2.5.0-RELEASE) and a few standalone console Powershell scripts connecting to that service.

### Usage

* launch application:
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```

* confirm with curl
```sh
curl --silent http://localhost:8080/
```
```json
{
  "timestamp": 1621727517750,
  "status": 401,
  "error": "Unauthorized",
  "message": "Full authentication is required to access this resource",
  "path": "/"
}
```
```sh
curl --silent --user admin:wrong_password http://localhost:8080/employees
```
```json
{
  "timestamp": 1621729150743,
  "status": 401,
  "error": "Unauthorized",
  "message": "Bad credentials",
  "path": "/employees"
}

```
```sh
curl -silent --user admin:password http://localhost:8080/employees
```
```json
{
  "timestamp": 1621729238168,
  "status": 404,
  "error": "Not Found",
  "message": "No message available",
  "path": "/employees"
}
```
```sh
curl --silent --user admin:password http://localhost:8080/employees/
```
```json
{
  "employees": [
    {
      "id": 1,
      "firstName": "Marc",
      "lastName": "McDonald",
      "email": "marcm@microsoft.com"
    }
  ]
}
```
alternatively use Powershell:
```powershell
. ./basic-auth_rest_client.ps1 -url http://localhost:8080/employees/
```
You will neeed to enter username and password via regular credentials dialog:
![credentials dialog](https://github.com/sergueik/springboot_study/blob/master/basic-basic-auth/screenshots/capture-auth.jpg)

### See Also:

  * [basics of ispring Security with basic authentication](https://www.baeldung.com/spring-security-basic-authentication)
  * [handling basic authentication with RestTemplate](https://www.baeldung.com/how-to-use-resttemplate-with-basic-authentication-in-spring)
  * https://www.javadevjournal.com/spring/basic-authentication-with-spring-security/
  * HTTP/1.1 [demos](https://jigsaw.w3.org/HTTP/) page demonstrating various common scenarios including basic auth
  * explanation of jigsaw *vintage* [document](https://www.w3.org/Jigsaw/Doc/User/authentication.html) 
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
