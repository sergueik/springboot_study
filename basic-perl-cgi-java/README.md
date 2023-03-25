### Info

Plain [OpenJDK alpine 3.9](https://hub.docker.com/layers/openjdk/library/openjdk/8-jre-alpine3.9/images/sha256-ea81da311d33e052eeea34336018434bdc50596100f9c623193867faa291b284) container with installed Perl 
with few pure Perl modules (YAML, XML and JSON)  
and a legacy CGI-BIN binary set 
and a Springboot application calling the legacy Perl in the background
for serving CGI-BIN pages

### Testing

* build the app
```sh
mvn package
```
* build the image
```sh
NAME=basic-perl-cgi-java-container
docker build -t $NAME -f Dockerfile .
```
* start run default command

```sh
docker container rm -f $NAME
docker run -d -p 8085:8085 --name $NAME $NAME
docker logs -f $NAME
```
this will respond with regular Springboot logo and aplication initialization messages and eventually
```text
 INFO [main] example.ExampleApplication               : Started ExampleApplication in 9.437 seconds (JVM running for 10.855)
```
* verify the vanilla httpd to run in Docker
```sh
curl http://localhost:8085/cgi-bin/example.cgi | jq '.'
```
on Windows Docker Toolbox, replace `localhost` with $DOCKER_MACHINE_IP

```sh
```

and `jq` with
```sh
export JQ='/c/tools/jq-win64.exe'
```
```sh
DOCKER_MACHINE_IP=$(docker-machine ip)
curl http://$DOCKER_MACHINE_IP:8085/cgi-bin/example.cgi | $JQ '.'
```


or (illustrating the parameter validation issue)
```sh
curl http://localhost:8085/bad/cgi-bin/example.cgi.dummy | jq '.'
```
this will print JSON:
```json
{
  "fruit": [
    "apple",
    "pear",
    "orange",
    "plum"
  ]
}
```

the log will show:
```txt
INFO 1 --- [io-8085-exec-10] example.service.ExampleService: Running the process: /var/www/localhost/cgi-bin//example.cgi -no-headers
```
With old SpringBoot __1.5.4-RELEASE__ if the 'dummy' extension is not added, the `PathVariable` seems to fail capturing the script name:
```sh
curl http://localhost:8085/bad/cgi-bin/example.cgi | jq '.'
```
show no output (there is currently no error handling in `ExampleService` class). The container console will show the error:
```txt
2021-06-12 17:03:10.298  INFO 1 --- [nio-8085-exec-2] example.controller.Controller: Running cgi-bin script: list
Running the process: /var/www/localhost/cgi-bin/example -no-headers
Process exit code: 2
<ERROR>Can't open perl script "/var/www/localhost/cgi-bin/example": No such file or directory</ERROR>
```
to fix this error modify
```java
@GetMapping(value = "/cgi-bin/bad/{script}"
public String bad(@PathVariable String script) {
...
```
to
```java
@GetMapping(value = "/cgi-bin/bad/{script:[a-z.0-9]+}"
public String fixed(@PathVariable String script) {
...
```
This issue  is not reproduced with SpringBoot __2.3.4-RELEASE__ or later releases. To reproduce the problem use
```sh
mvn -f pom-broken-pathvariable.xml clean package
```
* call cgi directly:
```sh
docker exec $NAME /var/www/localhost/cgi-bin/example.cgi -no-headers
```
replies with
```json
Content-Type: application/json

{
  "fruit": [
    "apple",
    "pear",
    "orange",
    "plum"
  ]
}

### Legacy Application Arguments
Some legacy `cgi-bin` apps do not follow `?key=value&...` format but expect simply a freehand commandline: `?-foo%20-bar=baz`. 
Spring RestController can access these arguments via `RequestEntity` and the web request
```sh
curl http://localhost:8085/cgi-bin/legacy-script?-a%20b%20-c%20d
```
will be translated to running cgi-bin script: `legacy-script` with `args[]` string array:
```sh
-a,b,-c,d
```
### Post Requests

```sh 
curl -H "Content-Type: application/json" -X POST -d '{"foo": 10, "bar": 30}' http://192.168.99.100:8085/cgi-bin/status2.cgi
```
this will print to the console
```text
Date: Sun, 19 Mar 2023 21:34:29 GMT
Content-Type: application/json
Content-Length: 56
 {   "foo" : 10,   "remote_addr" : "",   "bar" : 30}
```
```sh 
curl -H "Content-Type: application/json" -X POST -d '{"foo": "bar"}' http://localhost:8085/cgi-bin/status.cgi
```
this will print to the console
```text
Content-Type: application/json{   "remote_addr" : null,   "foo" : "bar"}
```
### Work in Docker ToolBox
if [Docker Toolbox](https://github.com/docker-archive/toolbox) is used from Windows host, find the ip address of the irtual Box container via the [command](https://devilbox.readthedocs.io/en/latest/howto/docker-toolbox/find-docker-toolbox-ip-address.html):
```sh
DOCKER_MACHINE_IP=$(docker-machine ip)
echo $DOCKER_MACHINE_IP
```
use `$DOCKER_MACHINE_IP` whenever accessing the container, instead of `localhost`.

### Shell Scripts

```sh
curl -s -X  GET -d '' -H 'Content-Type: application/json' http://$DOCKER_MACHINE_IP:8085/cgi-bin/static.sh | $JQ  '.' 
```
```JSON
{
  "data": {
    "rows": [
      {
        "name": "Ray",
        "message": "Hello",
        "imageUri": null,
        "_links": {
          "self": {
            "href": "http://localhost:8081/rows/1"
          },
          "link": {
            "href": "http://localhost:8081/rows/1"
          }
        }
      },
      {
        "name": "Charles",
        "message": "Hello",
        "imageUri": null,
        "_links": {
          "self": {
            "href": "http://localhost:8081/rows/1"
          },
          "link": {
            "href": "http://localhost:8081/rows/1"
          }
        }
      }
    ]
  }
}

```
```sh
curl -s -X  GET -d '{"data":[1, 2, 3]}' -H 'Content-Type: application/json' http://$DOCKER_MACHINE_IP:8085/cgi-bin/echo.sh | $JQ  '.'
```
```JSON
{
  "data": [
    1,
    2,
    3
  ]
  
```
### Handling  Failures
* invoke non existing scirpt
```sh
curl -H "Content-Type: application/json" -X POST -d '{"foo": 10, "bar": 30}' http://$DOCKER_MACHIME_IP:8085/cgi-bin/status3.sh
```
it will print custom JSON message:
```JSON
{
  "stdout": "",
  "exitcode": 2,
  "stderr": "Can't open perl script '/var/www/localhost/cgi-bin/status3.cgi': No such file or directory",
  "status": false
}
```
* test confirming collecting console and error messages

```sh
/var/www/localhost/cgi-bin/failing.sh 1>/dev/null
```
```text
error message
```
```sh
/var/www/localhost/cgi-bin/failing.sh 2>/dev/null
```
```text
console message
```
```sh
/var/www/localhost/cgi-bin/failing.sh  >& /dev/null
echo $?
```
```text
42
```

```sh
curl -H "Content-Type: application/json" -X POST -d '' http://$DOCKER_MACHINE_IP:8085/cgi-bin/failing.sh | $JQ
```
```JSON
{
  "stdout": "console message",
  "exitcode": 42,
  "stderr": "error message",
  "status": false
}
```
* NOTE: the code occasionally does not work when input is provided:
```sh
curl -H "Content-Type: application/json" -X POST -d '{"foo": "bar"}' http://$DOCKER_MACHINE_IP:8085/cgi-bin/failing.sh | $JQ
```
the console log shows
```text
2023-03-25 00:34:45.610  INFO 1 --- [nio-8085-exec-1] example.controller.Controller            : processing shell script: failing.sh
2023-03-25 00:34:45.616  INFO 1 --- [nio-8085-exec-1] example.service.ExampleService           : Running with environment: [CONTENT_LENGTH=14, REQUEST_METHOD=POST]
2023-03-25 00:34:45.646  INFO 1 --- [nio-8085-exec-1] example.service.ExampleService           : Passing the payload: {"foo": "bar"}
2023-03-25 00:34:45.649  INFO 1 --- [nio-8085-exec-1] example.service.ExampleService           : Exception (ignored): Broken pipe
2023-03-25 00:34:45.654  INFO 1 --- [nio-8085-exec-1] example.controller.Controller            : returning error from shell script: failing.sh
```
### Cleanup
```sh
docker container rm -f $NAME
docker image rm $NAME
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
