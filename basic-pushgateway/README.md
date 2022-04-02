### Info


this directory project is a replica of project based on plain Java (non-Spring)
[prometheus pushgateway client metric push demo application](https://github.com/shejoshi/pushgateway)
of Prometheus [pushgateway](https://github.com/prometheus/pushgateway) merged with Apache contgiguration allowing controling the "job" detals through arguments
and an Alpine docker containers with selected release installed

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

### Run with Vendor containers
### Prometheus

optionally create a separate network
```sh
export NETWORK=example_pushgateway
docker network create $NETWORK 
```

```sh
PROMETHEUS_VERSION=v2.27.0
docker run -p 9090:9090 --name=prometheus -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml --network=$NETWORK prom/prometheus:$PROMETHEUS_VERSION
```	
#### Pushgateway 

* run the `pushgateway` container explosing port `9091` to host:

```sh
PUSHGATEWAY_VERSION=v1.4.2
docker container rm pushgateway
docker run -d -p 9091:9091 --name=pushgateway --network=$NETWORK prom/pushgateway:$PUSHGATEWAY_VERSION
```

###  Run demo app locally
```sh
mvn package
```
post one fake job metrics (setting the last argument to `true` indicates if the job has failures )
```sh
for C in $(seq 1 1 5); do java -cp target/example.pushgateway-client.jar:target/lib/* example.App -s false --name "job name" --debug; sleep 10 ; done
```
* ignore the exception
```text
Missing argument: delay, using defult
Executing job: job name with status: success
get CollectorRegistry: 713338599
Set job duration timer: 1406718218
Set job info gauge: 245257410 [Name: job_info Type: GAUGE Help: Job identifier. Samples: []]
Exception (ignored): java.lang.NullPointerException
java.lang.NullPointerException
        at io.prometheus.client.Gauge.set(Gauge.java:265)
        at example.App.executeBatchJob(App.java:79)
        at example.App.main(App.java:181)
Executing job with status: success
Job complete
Set job success gauge: [Name: job_last_success Type: GAUGE Help: Last successful job run Samples: []]
Sending job info

```
* observe the metric placed:
```sh
curl -s http://localhost:9091/metrics | grep job

```
```text

# HELP job_duration_seconds Job duration in seconds.
# TYPE job_duration_seconds gauge
job_duration_seconds{instance="",job="job+name"} 3.003725464
# HELP job_info Job identifier.
# TYPE job_info gauge
job_info{instance="",job="job+name",more_info="more information",test_name="test name",test_suite="test suite"} 0
# HELP job_last_failure Last failed job run
# TYPE job_last_failure gauge
job_last_failure{instance="",job="job+name"} 1.648936468425e+09
# HELP job_last_success Last successful job run
# TYPE job_last_success gauge
job_last_success{instance="",job="job+name"} 1.648937581638e+09
push_failure_time_seconds{instance="",job="job+name"} 0
push_time_seconds{instance="",job="job+name"} 1.6489375817939155e+09
```
the pushed metrics will be visible on pushgateway web ui

![pushgateway page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/pushgateway_page_capture.png)

and prometheus web ui

![prometheus page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/prometheus_capture.png)

### Cleanup


```sh
docker container rm prometheus
docker container stop pushgateway
docker container rm pushgateway
docker network prune -f
```
### See Also

  * [usage documentation](https://prometheus.io/docs/instrumenting/pushing/)
  * [spring-boot example](https://github.com/ramesh-dev/prometheus-pushgateway-demo) - demonstrates configuring credentials parameters
  * https://www.reddit.com/r/docker/comments/m9l5k2/noob_question_what_is_the_difference_between/ 
  * Prometheus client libraries [metric types](https://prometheus.io/docs/concepts/metric_types/) - note `Info` is not covered
  * [Instrumenting Applications with Metrics for Prometheus](https://app.pluralsight.com/library/courses/instrumenting-applications-metrics-prometheus/table-of-contents) pluralsight course, mentions  producing `Info` metric and merging it with operational metric using orin in the __Recording application infomation using a custom metric__ slide.
  * [wget exit codes](https://gist.github.com/cosimo/5747881)
  * a similar [example](https://github.com/binhhq/prometheus-pushgateway-demo)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
