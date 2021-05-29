### Info

This directory contains code from [Spring boot security rest basic authentication example](https://howtodoinjava.com/spring-boot2/security-rest-basic-auth-example/) with tests temporarily disabled (the test needs SpringBoot parent version 2.5.0-RELEASE)

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
### See Also:

  * https://www.baeldung.com/spring-security-basic-authentication
  * https://www.javadevjournal.com/spring/basic-authentication-with-spring-security/
