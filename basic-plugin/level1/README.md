### Info

Loose Java app Docker basic example extracted from [Springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted too use alpine openjdk base image.

### Testing

* temporarily pull jdk base container.
```sh
TAG='8-jdk-alpine3.9'
docker pull openjdk:8-jdk-alpine3.9
```
* buld container . Note this involves compiling the Java code inside the container which may benot the best practice, if good at all (is there such a term as ultrabad practice ?)
 
```sh
IMAGE='basic-plugin'
docker build -f Dockerfile -t $IMAGE .
```
run
```sh
docker run --rm -it $IMAGE
```
which will print some message to console compiles on the container 
### Cleanup

```sh
docker container prune -f
docker image prune -f
docker image rm $IMAGE
docker image ls | grep $TAG | awk '{print $3}' | xargs -IX docker image rm X
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
