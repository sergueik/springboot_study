### Info

This project contains integration test for basic basic-auth project. It is not possibly to have 
the plain Junit intergaaton test is in the same project tree as Spring project due to the following collision:
if the main application is run via `mvn spring-boot:run` and   the test is run via `mvn integration-test` this woild trigger execiton of a regular unit tests
and it appears that if the `spring-boot:run` is executing the regular test will fail with

```text
IllegalState Failed to load ApplicationContext
```
 in every test
### Usage

```sh
mvn integration-test
```
### See Also

  * https://www.baeldung.com/java-http-request
  * [Authentication with HttpUrlConnectin](https://www.baeldung.com/java-http-url-connection)
  * https://stackoverflow.com/questions/64810671/spring-boot-random-sslexception-connection-reset-in-kubernetes-with-jdk11