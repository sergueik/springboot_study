### Info

Clone of [basic](https://github.com/sergueik/springboot_study/tree/master/basic) with more recent Springboot version for skeleton test work

### Noop Testing

```sh
curl -X POST -H 'Content-Type: application/json' -d '{"key":"value"}' -s http://localhost:8085/basic/hello
```
```JSON
{
  "service response":"Hello data"
}
```
```sh
curl -X POST -H 'Content-Type: application/json' -d '{"key":"value"}' -s http://localhost:8085/basic/hello/noop
```
```JSON
{
  "key": "value"
}
```
```sh
curl -X POST -H 'Content-Type: application/json' -d '{"key":"value"}' -s http://localhost:8085/basic/hello/true
```
```JSON
{
  "key": "value"
}
```
```sh
curl -X POST -H 'Content-Type: application/json' -d '{"key":"value"}' -s http://localhost:8085/basic/hello/false
```
```JSON
{
  "service response":"Hello data"
}
```
### See Also

  * https://reflectoring.io/unit-testing-spring-boot/
  * https://www.baeldung.com/injecting-mocks-in-spring
  * https://www.javadoc.io/doc/org.mockito/mockito-core/2.23.4/org/mockito/Mockito.html
  * [Assertions Generator](http://joel-costigliola.github.io/assertj/assertj-assertions-generator.html)
  * [springboot testing pyramid](https://github.com/kriscfoster/spring-boot-testing-pyramid) project featuring Unit, Integration & Acceptance tests for a simple Spring Boot REST controller (London).
  * [TDD classic v.mock wars](https://medium.com/@adrianbooth/test-driven-development-wars-detroit-vs-london-classicist-vs-mockist-9956c78ae95f)
  * [mocking Exception Throwing using Mockito](https://www.baeldung.com/mockito-exceptions) 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
