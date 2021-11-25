### Info

This directory contains code from [Spring boot security rest basic authentication example](https://howtodoinjava.com/spring-boot2/security-rest-basic-auth-example/) with tests temporarily disabled (the test needs SpringBoot parent version 2.5.0-RELEASE) and a few standalone console Powershell scripts connecting to that service.
updated with better security configuration borrowed from [admindebu/Spring-Boot-Restful-Basic-Auth-Security](https://github.com/admindebu/Spring-Boot-Restful-Basic-Auth-Security)
It is used as stub web server to test other apps.

### Usage

* launch application:
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```

* confirm with curl
```sh
curl --silent http://localhost:8080/
```
```text
HTTP Status 401 - Full authentication is required to access this resource
```
```sh
curl --silent --user admin:wrong_password http://localhost:8080/employees
```
```text
HTTP Status 401 - Bad credentials
```
```sh
curl -silent --user admin:password http://localhost:8080/employees
```
```json
{
"employees": [
{
  "id":1,
  "firstName":"Marc",
  "lastName":"McDonald",
  "email":"marcm@microsoft.com"
}
]}
```
alternatively use Powershell:
```powershell
. ./basic-auth_rest_client.ps1 -url http://localhost:8080/employees
```
You will neeed to enter username and password via regular credentials dialog:
![credentials dialog](https://github.com/sergueik/springboot_study/blob/master/basic-basic-auth/screenshots/capture-auth.jpg)

### TODO

Currently all 3 tests (no authentication,valid and invalid authentication) are failing with the same exception:
```text
401 : [HTTP Status 401 - Full authentication is required to access this resource]
```
### See Also:

  * [basics of spring Security with basic authentication](https://www.baeldung.com/spring-security-basic-authentication)
  * [overview of Spring Security](https://ru.wikibooks.org/wiki/Spring_Security/Технический_обзор_Spring_Security) (in Russian)
  * [handling basic authentication with RestTemplate](https://www.baeldung.com/how-to-use-resttemplate-with-basic-authentication-in-spring)
  * https://www.javadevjournal.com/spring/basic-authentication-with-spring-security/
  * HTTP/1.1 [demos](https://jigsaw.w3.org/HTTP/) page demonstrating various common scenarios including basic auth
  * explanation of jigsaw *vintage* [document](https://www.w3.org/Jigsaw/Doc/User/authentication.html) 
  * [Marc B. McDonald](https://en.wikipedia.org/wiki/Marc_McDonald) was Microsoft's first salaried employee
  * [basic testng springboot test](https://www.javainuse.com/spring/springboot_testng) (note: ad-heavy blog).

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
