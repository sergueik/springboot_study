### Info

this directory contains a replica of the

[hello-quarkus](https://github.com/bobcode99/hello-quarkus) repository combined  with OpeJDK 17 build containers

### Usage
* download the images
```sh
docker pull maven:3.8.3-openjdk-17
docker pull openjdk:17-alpine3.14
```
* build the JDK 17 container to build app
```sh
IMAGE=openjdk17-build
docker build -t $IMAGE -f Dockerfile.build .
```
* attempt to build the app
```sh
docker run -v $(pwd)/target:/app/target:rw -u root -it $IMAGE package

```
* confirm files are in place:
```sh
ls -l target/quarkus-app/
```
```text
total 24
drwxr-xr-x 2 root root 4096 May 29 18:25 app
drwxr-xr-x 4 root root 4096 May 29 18:25 lib
drwxr-xr-x 2 root root 4096 May 29 18:25 quarkus
-rw-r--r-- 1 root root 5453 May 29 18:25 quarkus-app-dependencies.txt
-rw-r--r-- 1 root root  653 May 29 18:25 quarkus-run.jar

```
* for accessing container from another shell
```sh
CONTAINER=$(docker container ls | grep  $IMAGE |awk '{print $1}')
```
```sh
docker build -f Dockerfile.app-alpine -t app .
```
```sh
docker run --entrypoint '' -p 8080:8080 -w /app -it app java -jar /deployments/quarkus-run.jar
```
shows logo
```text
__  ____  __  _____   ___  __ ____  ______
 --/ __ \/ / / / _ | / _ \/ //_/ / / / __/
 -/ /_/ / /_/ / __ |/ , _/ ,< / /_/ /\ \
--\___\_\____/_/ |_/_/|_/_/|_|\____/___/
2023-05-29 22:39:58,593 INFO  [io.quarkus] (main) hello-quarkus 1.0.0-SNAPSHOT n JVM (powered by Quarkus 2.11.1.Final) started in 1.002s. Listening on: http:/0.0.0.0:8080
2023-05-29 22:39:58,636 INFO  [io.quarkus] (main) Profile prod activated.
2023-05-29 22:39:58,637 INFO  [io.quarkus] (main) Installed feat
```
but does not process web requests:
```sh
curl -I http://localhost:8080/example/hello/World
```
```text
HTTP/1.1 404 Not Found
```
#### Cleanup
```sh
docker container prune
docker image prune
docker image rm app
docker image rm openjdk17-build
```
### See Also


  * https://quarkus.io/ .


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
