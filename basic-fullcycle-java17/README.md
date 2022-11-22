### Info

this is a replica of [example project](https://github.com/Diamis/docker-java)

it builds and runs java app in container started by `docker-compose`. The goal is to run java app as nonroot user and do not have any root ownerartifacts in the workspace after the completion

### Usage
*  build
```
./build.sh
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
### Cleanup

```sh
docker-compose stop
docker-compose rm -f
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
### See Also

   * https://blog.giovannidemizio.eu/2021/05/24/how-to-set-user-and-group-in-docker-compose/
   * https://nickjanetakis.com/blog/running-docker-containers-as-a-non-root-user-with-a-custom-uid-and-gid

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
