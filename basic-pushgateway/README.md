### Info

this directory contains a replica of plain Java [client](https://github.com/shejoshi/pushgateway) of Prometheus [path gateway](https://github.com/prometheus/pushgateway) app.
and an Alpine docker container with selected release installed

### Usage

* build vanilla alpine image and container with latest pushgateway with logging and history enabled through options
```sh
IMAGE=basic-pushgateway
docker build --build-arg VERSION=1.4.2 -t $IMAGE -f Dockerfile .
docker container rm $IMAGE
```
if getting an error code 8 from wget, or HTTP 404 if wget command run without a `-q` option
```text
ERROR 404: Not Found
```
review the releases by running
```sh
curl -s https://github.com/prometheus/pushgateway/releases | grep 'pushgateway/releases/download'  | grep linux-amd64
```
* run interactively
```sh
docker run --name $IMAGE -p 9091:9091 -it $IMAGE
```
* build console app in a separate console

```sh
mvn package
```
post one fake job metrics (setting the last argument to `true` indicates if the job has failures )
```sh
java -cp target/example.pushgateway-client.jar:target/lib/* example.App -s false --name "job name" --debug
```
The Java application logs "running the job" and "processing job failure":
			
```text
Missing argument: delay, using defult
Executing job: job name with status: failure
get CollectorRegistry: 713338599
Set job duration timer: 1406718218
Set job info gauge: 245257410 [Name: job_info Type: GAUGE Help: Job identifier. Samples: []]
Exception (ignored): java.lang.NullPointerException
Executing job with status: failure
Job is failing
Job Failure processing block
Set job failure timestamp gauge: 1705736037

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
 * Prometheus client libraries [metric types](https://prometheus.io/docs/concepts/metric_types/) - note `Info` is not covered
 * [Instrumenting Applications with Metrics for Prometheus]()pluralsight course, mentions  producing `Info` metric and merging it with operational metric using orin
in the __Recording application infomation using a custom metric__ slide.
  * [wget exit codes](https://gist.github.com/cosimo/5747881)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
