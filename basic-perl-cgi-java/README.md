### Info

Plain [OpenJDK alpine 3.9](https://hub.docker.com/layers/openjdk/library/openjdk/8-jre-alpine3.9/images/sha256-ea81da311d33e052eeea34336018434bdc50596100f9c623193867faa291b284) container with installed Perl 
with few pure Perl modules (YAML, XML and JSON)  
and  of a legacy CGI-BIN binary mockup 
and  a Springboot application calling the legacy Perl in the background
for serving CGI-BIN pages

### Testing

* build the appp
```
mvn package
```
* build the image
```sh
NAME=basic-perl-cgi-java-container
docker build -t $NAME -f Dockerfile .
```
* start run default command

```sh
docker run -d -p 8085:8085 --name $NAME $NAME
docker logs $NAME
```
this will respond with regular Springboot logo and eventually
```text
 INFO [main] example.ExampleApplication               : Started ExampleApplication in 9.437 seconds (JVM running for 10.855)
```
* verify the vanilla httpd to run in Docker
```sh
curl http://localhost:8085/cgi-bin/example.cgi | jq '.'
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
```
### Cleanup
```sh
docker container rm -f $NAME
docker image rm $NAME
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
