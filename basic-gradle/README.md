### Info

Springboot Docker basic project extracted from [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted to run on alpine openjdk jre base image.

* pull explicitly
```sh
docker pull gradle:7.3.1-jdk11-alpine
```
* build
```sh
IMAGE=gradle-build
docker build -t $IMAGE -f Dockerfile .
```
* run
```sh
docker container rm $IMAGE
docker run --rm -v $(pwd)/build:/work/build:rw -u root --name $IMAGE -it $IMAGE
```
* build
```sh
gradle build
```
on the host
```sh
ls build
```
```text
classes  generated  libs  resources  tmp
```

* run on the host
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
* verify

```sh
curl http://localhost:8085/basic/
```
```text
hello basic
```
* if need to troubleshoot

```sh
docker run -v $(pwd):build:/work/build:rw -u root --name $IMAGE -it $IMAGE sh
```
### Cleanup

```sh
docker container rm $IMAGE
docker image rm $IMAGE
docker volume prune -f
sudo rm -fr build 
mkdir build
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
