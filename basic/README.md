### Info

Springboot Docker basic project extracted from [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted to run on alpine openjdk jre base image.
### Test

* run locally
```sh
mvn clean spring-boot:run
```
* test locally
```sh
curl http://localhost:8085/basic
Hello basic
```
* run in container
```sh
IMAGE=basic-example
mvn clean package
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
docker run -p 8086:8085 $IMAGE
```
or
```sh
docker run -p $(hostname -i):8086:8085 $IMAGE
```
* NOTE: the `$(hostname -i):` argument was added as workaround of forced ipv6 switch
```sh
Error starting userland proxy: listen tcp6 [::]:8086:
socket: address family not supported by protocol
```
observed in Docker version __20.10.6__ on a host where ipv6 was [turned off](https://linuxconfig.org/how-to-disable-ipv6-address-on-ubuntu-18-04-bionic-beaver-linux)
* to also mount

```sh
CONTAINER=$(docker container ls | grep $IMAGE | awk '{print $1}')
DESTINATION=$(docker inspect $CONTAINER | jq -cr '.[]|.Mounts|.[]|.Destination')
docker exec -it $CONTAINER ls $DESTINATION
```
* test dockerized
```sh
curl http://localhost:8086/basic
Hello basic
```
destroy all started containers and image afterwards
```sh
docker container stop $(docker container ls |  grep $IMAGE | awk '{print $1}')
docker container prune -f
docker image prune -f
```
### Remote debugging

use `Dockerfile.DEBUG` instead of plain `Dockerfile` to pass the [debugging server arguments](https://dzone.com/articles/how-debug-remote-java-applicat) to `ENTTRYPOINT` and map the debugger port (currently hardcoded to __8998__) as:
```sh
docker build -f Dockerfile.DEBUG -t $IMAGE .
docker run -p 8085:8085  -p 8998:8998 $IMAGE
```
in the eclipse remote application debug configuration, use the IP address of the host
the launch configuration `.basic.launch` from `$HOME\workspace\.metadata\.plugins\org.eclipse.debug.core\.launches` looks like this:
```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<launchConfiguration type="org.eclipse.jdt.launching.remoteJavaApplication">
<listAttribute key="org.eclipse.debug.core.MAPPED_RESOURCE_PATHS">
<listEntry value="/basic"/>
</listAttribute>
<listAttribute key="org.eclipse.debug.core.MAPPED_RESOURCE_TYPES">
<listEntry value="4"/>
</listAttribute>
<booleanAttribute key="org.eclipse.jdt.launching.ALLOW_TERMINATE" value="true"/>
<mapAttribute key="org.eclipse.jdt.launching.CONNECT_MAP">
<mapEntry key="hostname" value="192.168.0.64"/>
<mapEntry key="port" value="8998"/>
</mapAttribute>
<stringAttribute key="org.eclipse.jdt.launching.PROJECT_ATTR" value="basic"/>
<stringAttribute key="org.eclipse.jdt.launching.VM_CONNECTOR_ID" value="org.eclipse.jdt.launching.socketAttachConnector"/>
</launchConfiguration>
```
### Running the Application With non-root User with Bound Writable Volume owned by the User

* use the application in this project to run the Docker container in the background with volume

* build image
```sh
export IMAGE=basic-app-user-volume
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
export NAME=basic-app-user-volume
docker run --name $NAME -d -v $(pwd)/test:/var/app/test:rw -p 8085:8085 $IMAGE
```
* create file from host
```sh
touch test/2.txt
```
* connect to container interactively

```sh
docker exec -it $IMAGE sh
```
* create file in the bound directory and view the permissions (in real scenario it can be e.g. static content or web page template):
```sh
touch test/1.txt
ls -l test
```
```text
total 0
-rw-rw-r--    1 user     1000             0 Nov  7 21:24 1.txt
-rw-r--r--    1 user     users            0 Nov  7 23:59 2.txt
```
```sh
exit
```
* verify
```sh
ls -ld test
```text
drwxrwxr-x 2 sergueik sergueik 4096 Nov  7 22:25 test/
```
ls -l test
```
```text
-rw-rw-r-- 1 sergueik sergueik 0 Nov  8 01:00 1.txt
-rw-r--r-- 1 sergueik users    0 Nov  8 01:00 2.txt
```
one can also try it through `docker-compose` using `docker-compose.basic-app-user-volume-UNTESTED.yaml` - not verified

#### See Also

  * [basic-user](https://github.com/sergueik/springboot_study/tree/master/basic-user) example
  * related [forum topic](https://qna.habr.com/q/1218548)(in Russian), no resolution

#### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
```
* Note: it appers that it will not be possible to disable the `ENTRYPOINT` in this example


### See Also

  * [step by step](https://github.com/in28minutes/SpringBootWebApplicationStepByStep) Web Application with Spring Boot
  * [package springboot as standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * [Test Strategies around Spring Boot](https://github.com/mechero/spring-boot-testing-strategies)
  * [REST Spring boot Unit tests](https://github.com/bytestree/spring-restful-service-unit-test)
  * Docker [command list](https://habr.com/ru/company/flant/blog/336654/) (in Russian)
  * deal with [failing ipv6](https://stackoverflow.com/questions/30750271/disable-ip-v6-in-docker-container) in Docker
  * [using Docker environment variables in build phase](https://vsupalov.com/docker-build-pass-environment-variables/)
  * [Docker commands](https://habr.com/ru/company/ruvds/blog/440660/) (in Russian)
  * official __Spring Boot Docker__ [documentation](https://spring.io/guides/topicals/spring-boot-docker/)
  * `@RequestParam` [basics](https://medium.com/@AADota/spring-passing-list-and-array-of-values-as-url-parameters-1ed9bbdf0cb2) of passing array of values via one parameter
  * [overiew](https://www.baeldung.com/spring-requestmapping) of __Spring RequestMapping__
  * pure Java generic list partition [methods](https://stackoverflow.com/questions/12026885/is-there-a-common-java-utility-to-break-a-list-into-batches) - not applicable for infrastructure partition
  * [introduction to JsonPath](https://www.baeldung.com/guide-to-jayway-jsonpath)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
