### Info

this is a replica of https://github.com/Diamis/docker-java

it builds and runs java app in containers started by `docker-compose`

### Usage
*  build
```
./build.sh
```
*  create cluster
```sh
docker-compose up
```
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

the build should be run by root

### See Also

   * https://blog.giovannidemizio.eu/2021/05/24/how-to-set-user-and-group-in-docker-compose/
   * https://nickjanetakis.com/blog/running-docker-containers-as-a-non-root-user-with-a-custom-uid-and-gid

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
