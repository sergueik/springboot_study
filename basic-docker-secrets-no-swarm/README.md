### Info

this directory contains the *docker secrets no swarm* [recipe](
https://serverfault.com/questions/871090/how-to-use-docker-secrets-without-a-swarm-cluster) test


### NOTE
the error after
```sh
docker-compose up
```
```txt
ERROR: The Compose file './docker-compose.yml' is invalid because:
Unsupported config option for services.secrets: 'secret_file'

```
indicates incorrect indentation in the `docker-compose.yml`: the `secrets` is both the `service` attribute and root element.
### Cleanup

```sh
docker container rm basic-docker-secrets-no-swarm_demo_service_1
```
### Home Brewed Vault

this example is a replica of the framework-less REST API in Java project covered in [tutorial](https://dev.to/piczmar_0/framework-less-rest-api-in-java-1jbl)
and [repo](https://github.com/piczmar/pure-java-rest-api/tree/step-6)
downgraded to JDK 1.8. The  cleanup and replacement of undesired dependencies is a work in progress
### Usage
```sh
mvn package
```
```sh
java -cp target/example.rest-api.jar:target/lib/* example.Application
```

* do the health check
```sh
curl -s -v 192.168.0.64:8000/api/hello?name=Test 
```
```text
Hello Test!* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 127.0.0.1:8000...
* Connected to localhost (127.0.0.1) port 8000 (#0)
> GET /api/hello?name=Test HTTP/1.1
> Host: localhost:8000
> User-Agent: curl/7.74.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Mon, 10 Oct 2022 03:00:38 GMT
< Content-length: 11
<
{ [11 bytes data]
* Connection #0 to host localhost left intact
```
* try to do registration but authentiate with the wrong password:
```sh
echo "admin:wrong password" | base64 -
YWRtaW46d3JvbmcgcGFzc3dvcmQK
```
then add header
```sh
curl -s -v -X POST localhost:8000/api/users/register -d '{"login": "test" , "password" : "test"}' -H 'Authorization: Basic YWRtaW46d3JvbmcgcGFzc3dvcmQK'
```
the server will log
```text
checking authentication: user: "admin" password:"wrong password
"
```
and return to the client

```text
* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 127.0.0.1:8000...
* Connected to localhost (127.0.0.1) port 8000 (#0)
> POST /api/users/register HTTP/1.1
> Host: localhost:8000
> User-Agent: curl/7.74.0
> Accept: */*
> Authorization: Basic YWRtaW46d3JvbmcgcGFzc3dvcmQK
> Content-Length: 39
> Content-Type: application/x-www-form-urlencoded
>
} [39 bytes data]
* upload completely sent off: 39 out of 39 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 401 Unauthorized
< Www-authenticate: Basic realm="myrealm"
< Date: Mon, 10 Oct 2022 03:01:45 GMT
< Content-length: 0
<
* Connection #0 to host localhost left intact
```
retry with correct credentials:
```sh
curl -s -v -X POST localhost:8000/api/users/register -d '{"login": "test" , "password" : "test"}' -H 'Authorization: Basic YWRtaW46YWRtaW4='
```
get the created `User` resource `id` echoed:
```text
{"id":"0d2b893b-d321-4985-abba-2d1b4b016c47"}* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 127.0.0.1:8000...
* Connected to localhost (127.0.0.1) port 8000 (#0)
> POST /api/users/register HTTP/1.1
> Host: localhost:8000
> User-Agent: curl/7.74.0
> Accept: */*
> Authorization: Basic YWRtaW46YWRtaW4=
> Content-Length: 39
> Content-Type: application/x-www-form-urlencoded
>
} [39 bytes data]
* upload completely sent off: 39 out of 39 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Mon, 10 Oct 2022 03:02:34 GMT
< Transfer-encoding: chunked
< Content-type: application/json
<
{ [51 bytes data]
* Connection #0 to host localhost left intact
```
### Getting The User of Specific Login Data Back

current version hosts an in-memory write-only `User` repository:
```java
public class User {
  String id;
  String login;
  String password;
}
```

* because the data is stored in memory need to post the user registration first
```sh
curl -s -X POST localhost:8000/api/users/register -d '{"login": "test" , "password" : "test"}' -H 'Authorization: Basic YWRtaW46YWRtaW4='
```
```json
{
  "id":"f78ae5e3-e8c2-4833-8da7-ed96d9ae022d"
}
```
```sh
curl -s -X GET localhost:8000/api/users/register?login=test -H 'Authorization: Basic YWRtaW46YWRtaW4='
```
```json
{
  "id": "f78ae5e3-e8c2-4833-8da7-ed96d9ae022d",
  "login": "test",
  "password": "test"
}
```

### TODO

* Replace __jackson.databind.ObjectMapper__  with __Gson__

### See Also

  * https://devops.stackexchange.com/questions/12101/secrets-in-docker-without-swarm
  * [official  documentation](https://docs.docker.com/compose/compose-file/#secrets)
  * for a rocket science solution, see [hashicorp vault](https://www.vaultproject.io)
  * another plain Java REST server [tutorial](https://dzone.com/articles/lightweight-embedded-java-rest-server-without-a-fr) and [demp project](https://github.com/StubbornJava/StubbornJava/tree/master/stubbornjava-examples/src/main/java/com/stubbornjava/examples/undertow/rest) - NOTE: non-maven source layout. Too much code
  * https://www.debugbear.com/basic-auth-header-generator
  * Lombok `@Value` annotation [examples](https://javabydeveloper.com/lombok-value-annotation-examples/)
  * [documentation](https://www.baeldung.com/lombok-builder) on usage of Lombok `@Builder` annotation
  * Lombok `@Builder` [examples](https://howtodoinjava.com/lombok/lombok-builder-annotation/)
  * Builder Design Pattern in Java [tutorial](https://www.digitalocean.com/community/tutorials/builder-design-pattern-in-java)
  * Lombok `@AllArgsConstructor` [examples](https://javabydeveloper.com/lombok-allargsconstructor-examples/)
  * [introduction to Vavr](https://www.baeldung.com/vavr)
  * [Vavr Tutorial](https://www.baeldung.com/vavr-tutorial)
  * [guide](https://www.baeldung.com/a-guide-to-java-enums) to Java enums
  * [guide](https://www.baeldung.com/java-enum-values) to manipilating values with Java enums

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
