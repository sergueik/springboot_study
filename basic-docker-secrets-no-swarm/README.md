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
java -cp target/pure-java-rest-api-1.0-SNAPSHOT.jar:target/lib/*  com.consulner.app.Application
```
try to authentiate with the wrong password:
```
echo "admin:wrong password" | base64 -
YWRtaW46d3JvbmcgcGFzc3dvcmQK
```
then add header
```
curl -v 192.168.0.64:8000/api/hello?name=Marcin -H 'Authorization: Basic YWRtaW46d3JvbmcgcGFzc3dvcmQK'
```
the server will log
```text
checking authentication: user: "admin" password:"wrong password
"
```
and return to the client
```text
* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 192.168.0.64:8000...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0* Connected to 192.168.0.64 (192.168.0.64) port 8000 (#0)
> GET /api/hello?name=Marcin HTTP/1.1
> Host: 192.168.0.64:8000
> User-Agent: curl/7.74.0
> Accept: */*
> Authorization: Basic YWRtaW46d3JvbmcgcGFzc3dvcmQK
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 401 Unauthorized
< Www-authenticate: Basic realm="myrealm"
< Date: Sat, 08 Oct 2022 21:23:43 GMT
< Content-length: 0
<
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
* Connection #0 to host 192.168.0.64 left intact
```
try correct credentials:
```sh
curl -v 192.168.0.64:8000/api/hello?name=Name -H 'Authorization: Basic YWRtaW46YWRtaW4='
```
get the parameter echo returned:
```text
curl -s -v 192.168.0.64:8000/api/hello?name=Name -H 'Authorization: Basic YWRtaW46YWRtaW4='
Hello Name!* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 192.168.0.64:8000...
* Connected to 192.168.0.64 (192.168.0.64) port 8000 (#0)
> GET /api/hello?name=Name HTTP/1.1
> Host: 192.168.0.64:8000
> User-Agent: curl/7.74.0
> Accept: */*
> Authorization: Basic YWRtaW46YWRtaW4=
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Sat, 08 Oct 2022 21:37:12 GMT
< Content-length: 11
<
{ [11 bytes data]
* Connection #0 to host 192.168.0.64 left intact
```

### See Also
  * https://devops.stackexchange.com/questions/12101/secrets-in-docker-without-swarm
  * [official  documentation](https://docs.docker.com/compose/compose-file/#secrets)
  * for a rocket science solution, see [hashicorp vault](https://www.vaultproject.io)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
