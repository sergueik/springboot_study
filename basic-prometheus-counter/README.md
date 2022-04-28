### Info


this directory contains a replica of a [simple Spring Boot application](https://github.com/arun-gupta/spring-boot-prometheus) that computes and exposes Prometheus metrics relying on springboot actuator to handle the endpoint that returns
prometheus metrics. Currently generates but does not export the appication specific metrics

### Usage

* run test
```sh
mvn test
```
* run application:

```sh
mvn spring-boot:run
```
* open the index page `http://localhost:8080/` in the browser or in the console, using `curl -s` to reduce the logging

![Sample Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-application.png)

```sh
curl -s http://localhost:8080/
```
replace `localhost` with the ip of the developer host if accessed from another
* follow the links or if run in console walk the application defined rotes

* access the application
```sh
for C in $(seq 1 1 5) ; do curl -s http://localhost:8080/hello > /dev/null; done
```
* run the "build" (there will be no output)
```sh
for C in $(seq 1 1 5) ; do curl -s http://localhost:8080/build ; done
```

the application console will log the metrics producting ativities
```text
2022-03-31 11:07:14.705  INFO 7352 --- [nio-8080-exec-2] example.controller.AppController         : incremented successful build counter
2022-03-31 11:07:18.778  INFO 7352 --- [nio-8080-exec-3] example.controller.AppController         : incremented successful build counter
2022-03-31 11:07:32.407  INFO 7352 --- [nio-8080-exec-4] example.controller.AppController         : increment requests_total
2022-03-31 11:07:32.410  INFO 7352 --- [nio-8080-exec-4] example.controller.AppController         : creating the timer
2022-03-31 11:07:32.416  INFO 7352 --- [nio-8080-exec-4] example.controller.AppController         : recording the requests_latency_seconds time duration
```
* access system metrics
```sh
curl http://localhost:8080/actuator/prometheus
```
will return
```text
# HELP requests_total Total number of requests.
# TYPE requests_total counter
requests_total 2.0
# HELP build_status_counter A simple Counter to illustrate custom build status and Prometheus
# TYPE build_status_counter counter
build_status_counter{status="error",} 1.0
build_status_counter{status="success",} 2.0
# HELP requests_latency_seconds Request latency in seconds.
# TYPE requests_latency_seconds histogram
requests_latency_seconds_bucket{le="0.005",} 2.0
requests_latency_seconds_bucket{le="0.01",} 2.0
requests_latency_seconds_bucket{le="0.025",} 2.0
requests_latency_seconds_bucket{le="0.05",} 2.0
requests_latency_seconds_bucket{le="0.075",} 2.0
requests_latency_seconds_bucket{le="0.1",} 2.0
requests_latency_seconds_bucket{le="0.25",} 2.0
requests_latency_seconds_bucket{le="0.5",} 2.0
requests_latency_seconds_bucket{le="0.75",} 2.0
requests_latency_seconds_bucket{le="1.0",} 2.0
requests_latency_seconds_bucket{le="2.5",} 2.0
requests_latency_seconds_bucket{le="5.0",} 2.0
requests_latency_seconds_bucket{le="7.5",} 2.0
requests_latency_seconds_bucket{le="10.0",} 2.0
requests_latency_seconds_bucket{le="+Inf",} 2.0
requests_latency_seconds_count 2.0
requests_latency_seconds_sum 9.373410000000001E-4
...
```

* access application defined metrics on `/metrics` to see the custom metrics

```sh
curl -s http://localhost:8080/metrics
```
will return
```text
# HELP requests_total Total number of requests.
# TYPE requests_total counter
requests_total 0.0
# HELP requests_latency_seconds Request latency in seconds.
# TYPE requests_latency_seconds histogram
requests_latency_seconds_bucket{le="0.005",} 0.0
requests_latency_seconds_bucket{le="0.01",} 0.0
...
requests_latency_seconds_bucket{le="+Inf",} 0.0
requests_latency_seconds_count 0.0
requests_latency_seconds_sum 0.0
```
along with custom  metrics
```text
# HELP cpu Value of metric from instance
# TYPE cpu gauge
cpu{instance="hostname06",domain="domain",environment="env",} 42.0
cpu{instance="hostname05",domain="domain",environment="env",} 42.0
cpu{instance="hostname08",domain="domain",environment="env",} 42.0
cpu{instance="hostname07",domain="domain",environment="env",} 42.0
cpu{instance="hostname09",domain="domain",environment="env",} 42.0
cpu{instance="hostname00",domain="domain",environment="env",} 42.0
cpu{instance="hostname02",domain="domain",environment="env",} 42.0
cpu{instance="hostname01",domain="domain",environment="env",} 42.0
cpu{instance="hostname04",domain="domain",environment="env",} 42.0
cpu{instance="hostname03",domain="domain",environment="env",} 42.0
```
there are few metric mockups: `load_average`, `cpu`, `disk` with random values

![Sample Mertrics Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-metrics.png)


### Feed to Prometheus

* place the app into Docker container
```sh
mvn package
```
```sh
export IMAGE=application
docker build -t $IMAGE -f Dockerfile .
```

```sh
NAME=application
docker container rm $NAME
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
* pull `prom2json` image. NOTE: pick a specific version to prevent polluting the images
```sh
VERSION=v1.3.0
docker pull prom/prom2json:$VERSION
```
* dump the metrics
```sh
docker run --link $NAME prom/prom2json:$VERSION http://$NAME:8080/metrics |jq
```
```json
[
  {
    "name": "requests_total",
    "help": "Total number of requests.",
    "type": "COUNTER",
    "metrics": [
      {
        "value": "2"
      }
    ]
  },
  {
    "name": "build_status_counter",
    "help": "A simple Counter to illustrate custom build status and Prometheus",
    "type": "COUNTER",
    "metrics": [
      {
        "labels": {
          "status": "error"
        },
        "value": "1"
      },
      {
        "labels": {
          "status": "success"
        },
        "value": "2"
      }
    ]
  },
  {
    "name": "requests_latency_seconds",
    "help": "Request latency in seconds.",
    "type": "HISTOGRAM",
    "metrics": [
      {
        "buckets": {
          "+Inf": "2",
          "0.005": "2",
          "0.01": "2",
          "0.025": "2",
          "0.05": "2",
          "0.075": "2",
          "0.1": "2",
          "0.25": "2",
          "0.5": "2",
          "0.75": "2",
          "1": "2",
          "10": "2",
          "2.5": "2",
          "5": "2",
          "7.5": "2"
        },
        "count": "2",
        "sum": "0.0009373410000000001"
      }
    ]
  }
]

```
alternatiely build if for alpine as shown in [basic-prom2json](https://github.com/sergueik/springboot_study/tree/master/basic-prom2json)
### NOTE

occasionally observerd challenge with numbered version image container to hang. 

```text
Error response from daemon: Get https://registry-1.docker.io/v2/: dial tcp: lookup registry-1.docker.io on [::1]:53: read udp [::1]:55749->[::1]:53: read: connection refused
```
Drop the `:v1.3.0` specified in this case and repeat

###  Connect metrics to Prometheus

* pull prometheus image
```sh
docker pull prom/prometheus:v2.27.0
```
* add configuration file `prometheus.yml` listing `application` host:
```yaml
scrape_configs:
  - job_name:       'node'

    # Override the global default and scrape targets from this job every 5 seconds.
    scrape_interval: 60s
    metrics_path: /metrics
    honor_labels: true

    static_configs:
      - targets: ['application:8080']
        labels:
          group: 'application'

```
* run
```sh
docker run --link application  -p 9090:9090  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus:v2.27.0
```
* open in the browser `http://192.168.0.64:9090/`
and click on __open the metrics explorer__ icon:

![Prometheus Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-prometheus.png)

one sees the metrics just added in the application code:
`build_status_counter`,`requests_total`,`requests_latency_seconds` 

Select a specic one e.g. `requests_latency_seconds_count`, sees the result:

```text
requests_latency_seconds_count{group="application", instance="application:8080", job="node"}
2
```
interact with application and see the counts getting updated

### Connect Application to Prometheus Directly
use the approach from [basic-pushgateway](https://github.com/sergueik/springboot_study/tree/master/basic-pushgateway) project:

* add the following onfiguration to  prometheus:

```YAML
- job_name: application
  honor_labels: true
  honor_timestamps: true
  scrape_interval: 15s
  scrape_timeout: 1s
  metrics_path: /metrics
  scheme: http
  follow_redirects: true
  static_configs:
  - targets:
    - application:8080
```
```sh
NETWORK='example_pushgateway'
docker network create $NETWORK

NAME=application
export IMAGE=application
docker run --network=$NETWORK --name $NAME  -p 8080:8080 -d $IMAGE

PROMETHEUS_VERSION=v2.27.0
docker run -d -p 9090:9090 --name=prometheus -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml --link application --network=$NETWORK prom/prometheus:$PROMETHEUS_VERSION
```
### Cleanup

```sh
docker container rm -f $NAME
docker container prune -f
docker image rm $IMAGE
docker image rm prom/prom2json
docker image rm prom/prom2json:v1.3.0
docker image rm prom/prometheus:v2.27.0
```
### See Also

  * Prometheus [prom2json](https://hub.docker.com/r/prom/prom2json) Docker hub link
  * Prometheus [prometheus](https://hub.docker.com/r/prom/prometheus) Docker ub lik
  * [minimal prometheus counter example](https://github.com/njanor/spring-boot-prometheus-demo) - uses older Springboot and currently does not compile, integrated the `build status` counter from the example
  * [step by step springboot monitoring dashboard example](https://github.com/rishant/springboot-monitoring-dashboard-example)
  * [example](https://github.com/ramesh-lingappan/prometheus-pushgateway-demo) - demonstrates `PushGatewayConfiguration`  and `PushGatewayCredentials` bolierplate code
  * old [MVC example](https://github.com/ConSol/springboot-monitoring-example) with static page links to generated metrics, the application handles the `/metrics` route on its own.
  * Spring Boot metrics monitoring using Prometheus & Grafana [blog](https://aboullaite.me/spring-boot-monitoring-prometheus-grafana/) - relies on `@EnablePrometheusEndpoint` and `@EnableSpringBootMetricsCollector` annotations which do not work yet in this project
  * core [metrics Collection in Spring Boot With Micrometer and Prometheus](https://www.codeprimers.com/metrics-collection-in-spring-boot-with-micrometer-and-prometheus/) documentatiton
  * [collection of alerting rules for Prometheus](https://awesome-prometheus-alerts.grep.to)
  * plain Java (non-Spring) [prometheus pushgateway metric push demo application](https://github.com/binhhq/prometheus-pushgateway-demo)
  * https://www.tabnine.com/code/java/methods/io.prometheus.client.Collector$MetricFamilySamples$Sample/%3Cinit%3E
  * https://www.tabnine.com/code/java/methods/io.prometheus.client.CollectorRegistry/register
  * https://prometheus.github.io/client_java/io/prometheus/client/Collector.MetricFamilySamples.Sample.html
  * https://prometheus.github.io/client_java/io/prometheus/client/Collector.MetricFamilySamples.html
  * Prometheus [metric types](https://prometheus.io/docs/concepts/metric_types/)  

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
