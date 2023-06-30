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

### Gradle Build
* pull explicitly
```sh
docker pull gradle:7.3.1-jdk11-alpine
```
* build
```sh
IMAGE=gradle-build
DOCKER build -t $IMAGE -f Dockerfile.gradle
```
* run
```sh
docker run -u root --name $IMAGE -it $IMAGE sh
```
* remove test to avoid massive dependency pull
```sh
rm -fr src/test/
```
* build
```sh
gradle build
```
* test
```sh
java -jar build/libs/basic-0.1.0-SNAPSHOT.jar
```

```text

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)

2023-06-30 01:11:03.281  INFO 188 --- [           main] example.ExampleApplicati
on               : Starting ExampleApplication on a10dca1b0b77 with PID 188 (/wo
rk/build/libs/basic-0.1.0-SNAPSHOT.jar started by root in /work)
2023-06-30 01:11:03.320  INFO 188 --- [           main] example.ExampleApplicati
on               : No active profile set, falling back to default profiles: defa
ult
2023-06-30 01:11:15.087  INFO 188 --- [           main] o.s.b.w.embedded.tomcat.
TomcatWebServer  : Tomcat initialized with port(s): 8085 (http)
2023-06-30 01:11:15.211  INFO 188 --- [           main] o.apache.catalina.core.S
tandardService   : Starting service [Tomcat]
2023-06-30 01:11:15.213  INFO 188 --- [           main] org.apache.catalina.core
.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
2023-06-30 01:11:15.877  INFO 188 --- [           main] o.a.c.c.C.[Tomcat].[loca
lhost].[/]       : Initializing Spring embedded WebApplicationContext
2023-06-30 01:11:15.884  INFO 188 --- [           main] w.s.c.ServletWebServerAp
plicationContext : Root WebApplicationContext: initialization completed in 11914
 ms
2023-06-30 01:11:20.851  INFO 188 --- [           main] o.s.s.concurrent.ThreadP
oolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2023-06-30 01:11:23.079  INFO 188 --- [           main] o.s.b.w.embedded.tomcat.
TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
2023-06-30 01:11:23.200  INFO 188 --- [           main] example.ExampleApplicati
on               : Started ExampleApplication in 25.293 seconds (JVM running for
 30.536)
```
* TODO:

* does not work with gradle __5.x__
```sh
gradle build
```
```text
> Task :compileJava UP-TO-DATE
> Task :processResources UP-TO-DATE
> Task :classes UP-TO-DATE
> Task :findMainClass FAILED

FAILURE: Build failed with an exception.

* What went wrong:
Execution failed for task ':findMainClass'.
> org.gradle.api.tasks.SourceSetOutput.getClassesDir()Ljava/io/File;

```
* no dependency packed:
```sh
ls -l build/libs
```
```text
total 112
-rw-r--r--    1 root     root         98342 Jun 30 00:14 basic-0.1.0-SNAPSHOT.ja
r
```
attempt to run 
```sh
java -jar build/libs/basic-0.1.0-SNAPSHOT.jar
```
 fails with
```text
Exception in thread "main" java.lang.reflect.InvocationTargetException
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.
java:62)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAcces
sorImpl.java:43)
        at java.lang.reflect.Method.invoke(Method.java:498)
        at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner
.java:48)
        at org.springframework.boot.loader.Launcher.launch(Launcher.java:87)
        at org.springframework.boot.loader.Launcher.launch(Launcher.java:50)
        at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:51)
Caused by: java.lang.NoClassDefFoundError: org/springframework/boot/SpringApplic
ation
        at example.ExampleApplication.main(ExampleApplication.java:12)
        ... 8 more
Caused by: java.lang.ClassNotFoundException: org.springframework.boot.SpringAppl
ication

```

* does not work with gradle __8.x__
```

* What went wrong:
org/gradle/api/plugins/MavenPlugin
```
- need to [replace plugin](https://stackoverflow.com/questions/69949638/the-legacy-maven-plugin-was-removed-in-gradle-7-please-use-the-maven-publish)

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
  * [how to](https://www.digitalocean.com/community/tutorials/how-to-install-docker-compose-on-ubuntu-18-04) install Docker Compose from the Docker’s GitHub repository  
  * [docker routing](https://codepoetry.ru/post/docker-user-iptables/)(in Russian)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
