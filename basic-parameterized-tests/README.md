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


curl -s -X POST http://localhost:8085/typed/data -d '{"name": "name", "status": false }' -H 'Content-Type: application/json'
"{\"status\":false,\"name\":\"name\"}"
Serguei@sergueik53 MINGW64 /c/developer/sergueik/springboot_study/basic-tests (master)
$ curl -s -X POST http://localhost:8085/typed/data -d '{"name": "name", "status": null }' -H 'Content-Type: application/json'
"{\"status\":false,\"name\":\"name\"}"

curl -s -X POST http://localhost:8085/typed/map -d '{"name": "name", "status": null }' -H 'Content-Type: application/json'
"{\"name\":\"name\"}"
Serguei@sergueik53 MINGW64 /c/developer/sergueik/springboot_study/basic-tests (master)
$ curl -s -X POST http://localhost:8085/typed/map -d '{"name": "name", "status": false }' -H 'Content-Type: application/json'
"{\"name\":\"name\",\"status\":\"false\"}"


### Status Code Tests

* the `/basic/statuscode` endpoint can be used to stub legacy application failing with specific HTTP statuses:

```sh
curl -Is http://localhost:8085/basic/statuscode?code=404
```
```text
HTTP/1.1 404 
Content-Length: 0
Date: Wed, 17 Apr 2024 17:54:15 GMT
```
```sh
curl -Is http://localhost:8085/basic/statuscode?code=403
```
```text
HTTP/1.1 403 
Content-Length: 0
Date: Wed, 17 Apr 2024 17:54:17 GMT
```
```sh
curl -Is http://localhost:8085/basic/statuscode?code=503
```
```text
HTTP/1.1 503 
Content-Length: 0
Date: Wed, 17 Apr 2024 18:54:22 GMT
Connection: close
```
NOTE: for non-standard values of `code` it will produce a status `500`
```sh
curl -Is http://localhost:8085/basic/statuscode?code=999
```

```text
HTTP/1.1 500 
Content-Type: application/json;charset=UTF-8
Transfer-Encoding: chunked
Date: Wed, 17 Apr 2024 17:54:21 GMT
Connection: close
```
with server logs containing the exception:
```text
java.lang.IllegalArgumentException: No matching constant for [999]
	at org.springframework.http.HttpStatus.valueOf(HttpStatus.java:538)
```
```
NOTE: omitting the `code` query parameter argument

```sh
curl -Is http://localhost:8085/basic/statuscode
```
is leading to status `400` (Bad Request)
```text
HTTP/1.1 400 
Content-Type: application/json;charset=UTF-8
Transfer-Encoding: chunked
Date: Wed, 17 Apr 2024 18:08:31 GMT
Connection: close

```
with server side log:
```text
org.springframework.web.bind.MissingServletRequestParameterException: 
Required int parameter 'code' is not present
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
