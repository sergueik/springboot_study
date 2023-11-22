### Info

this is a replica of [example project](https://github.com/Diamis/docker-java)

it builds and runs java app in container started by `docker-compose`. The goal is to run java app as nonroot user and do not have any root ownerartifacts in the workspace after the completion

### Prerequisites

if the `docker-compose` installed  by `apt-get` it is too old. However it is OK to install `docker-compose` via apt-get first for dependency sake. NOTE: the download instruction from [digitalocean](https://www.digitalocean.com/community/tutorials/how-to-install-docker-compose-on-ubuntu-18-04) modified to put the script into `/usr/bin` instead of `/usr/local/bin`
```sh
VERSION=2.14.0
sudo curl -sL https://github.com/docker/compose/releases/download/v$VERSION/docker-compose-`uname -s`-`uname -m` -o docker-compose
sudo chmod 775 docker-compose
sudo cp docker-compose /usr/bin/docker-compose
```

### Usage

* pull images

```sh
docker pull maven:3.8.3-openjdk-17
docker pull openjdk:17
docker pull openjdk:17-alpine3.14
```
it is over 1Gb worth of disk, counting build and run containers
```text
maven            3.8.3-openjdk-17        0b9ddcb8259e   12 months ago   785MB
openjdk          17                      5e28ba2b4cdb   6 months ago    471MB
openjdk          17-alpine               264c9bdce361   17 months ago   326MB
```

*  build
```sh
./build.sh | tee build.log
```
this runs `Dockerfile.build` as non-root user to create the maven `target` directory  in the project directory (does not like it to owned by the root user), followed by constructing a skeleton Java 17 container with similar non-root user to feed to `docker-compose`

*  create cluster
```sh
docker-compose up
```
this runs the java 17 app as `docker-compose` service mounted the current directory of workspace as applicaation writable `/app` dir.
* interact with app
```sh
curl http://localhost:8585
```
check file attributes
```sh
stat *txt
```
* if 
```text
Access: (0644/-rw-r--r--)  Uid: (    0/    root)   Gid: (    0/    root)
```
the docker is misconfigured
* the expected is
```text
access: (0644/-rw-r--r--)  Uid: ( 1000/sergueik)   Gid: ( 1000/sergueik)
```
* interact with app running in alpine container
```sh
curl http://localhost:8586
```
the log will be created


### Cleanup

```sh
docker-compose stop
docker-compose rm -f
```
the explicit image cleanup appears to be required:

```sh
 docker image ls | grep sample-java17| awk '{print $3}' | xargs -IX docker image rm X
```
### NOTE 

the `/usr/local/bin/mvn-entrypoint.sh` in the image `maven:3.8.3-openjdk-17`
is invoked and proudces the warning messages:
```text
mkdir: cannot create directory ‘/root’: Permission denied
```
```text
Can not write to /root/.m2/copy_reference_file.log. Wrong volume permissions? Carrying on ...

```
* inspect the image
```sh
docker inspect maven:3.8.3-openjdk-17 | jq '.[]|.Config.Env'

```
```json
[
  "PATH=/usr/java/openjdk-17/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  "JAVA_HOME=/usr/java/openjdk-17",
  "LANG=C.UTF-8",
  "JAVA_VERSION=17.0.1",
  "MAVEN_HOME=/usr/share/maven",
  "MAVEN_CONFIG=/root/.m2"
]

```
```sh
 docker inspect maven:3.8.3-openjdk-17 | jq '.[]|.Config.Entrypoint'
```
```json
[
  "/usr/local/bin/mvn-entrypoint.sh"
]

```

the build should normally be run by root, is is tweaked to have `target` mapped

### TODO

update `pom.xml` and container configurations to have pinned jar name
```xml
<configuration>
  <finalName>${finalName}</finalName>
</configuration>
```
### Strongly-Typed Request Processing
```sh
curl -s -X POST -d '{"name": "some", "status": null}' -H 'Content-Type: application/json' http://localhost:8586/typed/data
```
```JSON
{"status":false,"name":"some"}
```
```sh
curl -s -X POST -d  '{"name": "some", "status": null}' -H 'Content-Type: application/json' http://localhost:8586/typed/map
```
```JSON
{"name":"some"}
```
```sh
curl -s -X POST -d '{"name": "some", "status": "abcde","street": "unknown"}' -H 'Content-Type: application/json' http://localhost:8586/typed/data
```
```text
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8586 (#0)
> POST /typed/data HTTP/1.1
> Host: localhost:8586
> User-Agent: curl/7.58.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 55
> 
* upload completely sent off: 55 out of 55 bytes
< HTTP/1.1 400 
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Wed, 22 Nov 2023 20:03:09 GMT
< Connection: close
< 
* Closing connection 0
{"timestamp":"2023-11-22T20:03:09.139+00:00","status":400,"error":"Bad Request","path":"/typed/data"}
```
the error logged will be:
```text
w.s.m.s.DefaultHandlerExceptionResolver : Resolved 
[org.springframework.http.converter.HttpMessageNotReadableException: 
JSON parse error: Cannot deserialize value of type `boolean` 
from String "abcde": only "true"/"True"/"TRUE" or "false"/"False"/"FALSE" 
recognized; nested exception is com.fasterxml.jackson.databind.exc.InvalidFormatException: 
Cannot deserialize value of type `boolean` from String "abcde": only "true"/"True"/"TRUE" or "false"/"False"/"FALSE" recognized<EOL> at [Source: (org.springframework.util.StreamUtils$NonClosingInputStream); line: 1, column: 28] (through reference chain: com.example.demo.TypedController$Data["status"])

```
### See Also

   * https://blog.giovannidemizio.eu/2021/05/24/how-to-set-user-and-group-in-docker-compose/
   * https://nickjanetakis.com/blog/running-docker-containers-as-a-non-root-user-with-a-custom-uid-and-gid

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
