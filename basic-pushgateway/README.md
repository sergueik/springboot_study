### Info

this directory contains a replica of plain Java [client](https://github.com/shejoshi/pushgateway) of Prometheus [path gateway](https://github.com/prometheus/pushgateway) app.
and an Alpine docker container with selected release installed

### Usage

* build vanilla alpine image and container with latest pushgateway with logging and history enabled through options
```sh
IMAGE=basic-pushgateway
docker build --build-arg VERSION=1.2.4 -t $IMAGE -f Dockerfile .
docker container rm $IMAGE
```
run interactively
```sh
docker run --name $IMAGE -p 9091:9091 -it $IMAGE
```
* build console app

```sh
mvn package
```
post one fake job metrics (setting the last argument to `true` indicates if the job has failures )
```sh
java :-cp target/pushgateway-client-0.1.0-SNAPSHOT.jar:target/lib/* example.App false
```
This will log in the container console
```sh
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2021-10-27T02:57:19.671Z"}
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2021-10-27T02:58:03.371Z"}
```
### See Also

 * [usage documentation](https://prometheus.io/docs/instrumenting/pushing/)
 * [spring-boot example](https://github.com/ramesh-dev/prometheus-pushgateway-demo) - demonstrates configuring credentials parameters
 * https://www.reddit.com/r/docker/comments/m9l5k2/noob_question_what_is_the_difference_between/ 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
