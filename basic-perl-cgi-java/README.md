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
* ver	ify the vanilla httpd to run in Docker
```sh
curl http://localhost:8085/cgi-bin/example.cgi | jq '.'
```
on Windows __Docker Toolbox__, replace `localhost` with `$DOCKER_MACHINE_IP`. NOTE: this environment variable may not be set even in __Docker Toolbox__ shell
and will definitely not be set outside. To evauate run
```
export DOCKER_MACHINE_IP=$(docker-machine ip)
```
The `docker-machine` will be in the `$PATH` in __Docker Toolbox__ shell

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
2023-03-25 20:46:42.816  INFO 1 --- [nio-8085-exec-9] example.controller.Controller            : processing shell script: failing.sh
2023-03-25 20:46:42.826  INFO 1 --- [nio-8085-exec-9] example.utils.ProcessRunner              : Process exit code: 42
2023-03-25 20:46:42.828  INFO 1 --- [nio-8085-exec-9] example.utils.ProcessRunner              : <OUTPUT>console message</OUTPUT>
2023-03-25 20:46:42.833  INFO 1 --- [nio-8085-exec-9] example.utils.ProcessRunner              : <ERROR>error message</ERROR>
2023-03-25 20:46:42.836  INFO 1 --- [nio-8085-exec-9] example.service.ExampleService           : returning error from command: /var/www/localhost/cgi-bin/failing.sh
```

### Config

* NOTE: the Java app is not currently creating `$QUERY_STRING`.

```sh
curl http://$DOCKER_MACHINE_IP:8085/cgi-bin/config.cgi
```
```JSON
{
  "stdout": "",
  "exitcode": 2,
  "stderr": "Use of uninitialized value $query_string in split at /var/www/localhost/cgi-bin/config.cgi line 66.Can't call method 'mtime' on an undefined value at /var/www/localhost/cgi-bin/config.cgi line 46.",
  "status": false
}
	
```
```sh
curl http://$DOCKER_MACHINE_IP:8085/cgi-bin/file_hash.cgi
```
```text
Content-Type: application/json
```
```JSON
{  
  "result" : "Config //example_config.json not found",  
  "status" : "error"
}
```
```
curl http://$DOCKER_MACHINE_IP:8085/cgi-bin/file_hash_status.cgi
```
```text
{
  "stdout": "Status: 500 Internal Server Error Date: Thu, 05 Oct 2023 22:19:53 GMT Content-Type: text/plain Content-Length: 25 500 Internal Server Error",
  "exitcode": 255,
  "stderr": "Unknown debug command -no-headers cgi exited without rendering a response",
  "status": false
}
```
### Cleanup
```sh
docker container rm -f $NAME
docker image rm $NAME
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
