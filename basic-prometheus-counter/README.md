### Info


this directory contains a replica of a [simple Spring Boot application](https://github.com/arun-gupta/spring-boot-prometheus) that computes and exposes Prometheus metrics relying on springboot actuator to handle the endpoint that returns
prometheus metrics. Currently generates but does not export the appication specific metrics

### Usage

* rebuild and "install" the `springboot_study/prometheus_simpleclient_common`:

```sh
pushd ~/src/springboot_study/prometheus_simpleclient_common
mvn install
popd
```
this is a dependency of __0.26.0-SNAPSHOT__, __0.27.0-SNAPSHOT__ - may not be needed for later versions

Create the sqlite database on Desktop `springboot.db`

```sh
pushd ~
sqlite3 Desktop/springboot.db
```
with a table
```sql
DROP TABLE IF EXISTS `hosts`;
CREATE TABLE "hosts" ( `id` INTEGER, `hostname` TEXT NOT NULL, `app` TEXT, `environment` TEXT, `domain` TEX, `addtime` TEXT, PRIMARY KEY(`id`) );
.quit
```
update `src/main/resources/application.properties` to point to it:
```java
spring.datasource.url=jdbc:sqlite:${HOME}/Desktop/springboot.db
```
for Linux host
and with

```java
spring.datasource.url=jdbc:sqlite:${USERPROFILE}\\Desktop\\springboot.db
```
for Windows host
and insert some data
```sql
insert into hosts(hostname,app,environment,domain) values('hostname00','redis','qa','west');
insert into hosts(hostname,app,environment,domain) values('hostname01','redis','prod','east');
.quit
```
alternatively use [SQLIteBrowser](https://sqlitebrowser.org)
* verify
```powershell
sqlite3.exe $env:userprofile\Desktop\springboot.db
```
```text
.table hosts
```
```sh
.schema hosts
```

```sql
select * from hosts;
```
* run test
```sh
mvn test
```
* run application:

```sh
mvn -Dmaven.test.skip=true spring-boot:run
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
# HELP cpu Value of metric from instance
# TYPE cpu gauge
cpu{instance="hostname00",domain="west",app="database",} 42.0
cpu{instance="hostname04",domain="west",app="node",} 16.0
cpu{instance="hostname07",domain="east",app="node",} 4.0
cpu{instance="hostname03",domain="west",app="redis",} 20.0
cpu{instance="hostname08",domain="west",app="redis",} 38.0
```
there are few metric mockups: `load_average`, `cpu`, `disk` with random values
The `domain` and `app` are read from cluster configuration YAML:
```YAML
---
  - host:
    id: 2
    hostname: hostname00
    dc: west
    app: database
  - host:
    id: 3
    hostname: hostname03
    dc: west
    app: redis
  - host:
    id: 4
    hostname: hostname04
    dc: west
    app: node
  - host:
    id: 5
    hostname: hostname05
    dc: west
    app: ~
  - host:
    id: 6
    hostname: hostname06
    dc: west
    app: ~
  - host:
    id: 7
    hostname: hostname07
    dc: west
    app: node
  - host:
    id: 8
    hostname: hostname07
    dc: east
    app: node
  - host:
    id: 9
    hostname: hostname08
    dc: west
    app: redis
```

![Sample Metrics Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-metrics.png)


![Sample Servers Inventory Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-servers.png)

for testing purposes we also have the API endpoint to show the data for selected host, filtered to show specific metrfic names
```sh
HOSTNAME=hostname00
curl -s http://localhost:8080/hostdata/$HOSTNAME | jq '.'
```

```json
{
  "hostname": "hostname00",
  "data": {
    "disk": "40.5",
    "memory": "20",
    "load_average": "6",
    "cpu": "10",
    "rpm": "100"
  }
}
```
![Sample HostData Page](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture_hostdata_one_host.png)

### Feed to Prometheus

* adjust `src/main/resources/application.properties` to development host platform:
uncomment
for linux
```java
spring.datasource.url=jdbc:sqlite:${HOME}/Desktop/springboot.db
```
uncomment
for windows
```java
spring.datasource.url=jdbc:sqlite:${USERPROFILE}\\Desktop\\data.db
```
for docker
```java
spring.datasource.url=jdbc:sqlite:/demo/src/test/resources/data.db
```
* place the app into Docker container
```sh
mvn -Dmaven.test.skip=true clean package
```
```sh
export IMAGE=application
docker build -t $IMAGE -f Dockerfile .
```

```sh
NAME=application
docker container rm -f $NAME
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
```sh
docker logs $NAME
```
confirm the successul launch
```text
2022-04-29 23:58:56.422  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
```

```text
: Started Application in 23.396 seconds (JVM running for 25.76)
```
 it the exception is logged, most likely the `applocation.properies` were not updated a should

* confirm the 'monitoring data' is present on application container'
```sh
docker exec $NAME find /demo/src/test/resources/data -type f
```
```text
/demo/src/test/resources/data/hostname04/data.txt
/demo/src/test/resources/data/hostname03/data.txt
/demo/src/test/resources/data/hostname01/data.txt
/demo/src/test/resources/data/hostname05/data.txt
/demo/src/test/resources/data/hostname00/data.txt
/demo/src/test/resources/data/hostname08/data.txt
/demo/src/test/resources/data/hostname02/data.txt
/demo/src/test/resources/data/hostname07/data.txt
/demo/src/test/resources/data/hostname06/data.txt
```

The application will serve  endpoints: `/metrics`, `instantmetrics`  and `staticmetrics`

The latter two write a static page with dummy `memory` values and a current time and a past time stamp (converted to milliseconds epoch)
```sh
curl -s http://192.168.0.29:8080/instantmetrics
```
```text
# HELP memory Value of metric from instance
# TYPE memory gauge
memory{instance="hostname00",datacenter="dummy",application="application01",linborg_instance="instance01",} 100.0 1657227769814
memory{instance="hostname01",datacenter="dummy",application="application01",linborg_instance="instance03",} 100.0 1657227769814
memory{instance="hostname01",datacenter="dummy",application="application01",linborg_instance="instance04",} 100.0 1657227769814
memory{instance="hostname01",datacenter="dummy",application="application02",linborg_instance="instance05",} 100.0 1657227769814
memory{instance="hostname00",datacenter="dummy",application="application01",linborg_instance="instance02",} 100.0 1657227769814
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

NOTE, the `prom2json` will fail to accept `instantmetrics` and `staticmetrics`.

alternatiely build if for alpine as shown in [basic-prom2json](https://github.com/sergueik/springboot_study/tree/master/basic-prom2json)
### NOTE

when building package for container, make sure to temporarily update the `application.properties`:
```java
spring.datasource.url=jdbc:sqlite:/demo/src/test/resources/data.db
```
and package skipping the tests - the database location is different between desktop and container runs
```sh
mvn -Dmaven.test.skip=true clean package
```
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
* run with the [option flag required](https://www.robustperception.io/reloading-prometheus-configuration) to enable the `http://localhost:9090/-/reload` endpoint
```sh
docker stop $(docker container ls | grep prom/prometheus | awk '{print $1}')
```
```sh
docker run --link application  -p 9090:9090  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus:v2.27.0 --web.enable-lifecycle --config.file=/etc/prometheus/prometheus.yml
```
- note that together with `web.enable-lifecycle` one has to provide the `config.file` argument, to avoid  the error
```text
msg="Error loading config (--config.file=prometheus.yml)" err="open prometheus.yml: no such file or directory"
```
* NOTE when `prometheus.yml` is mapped via volume, changes made to the file in current directory after the container is starter may not propagate in `/etc/prometheus/prometheus.yml` - copy explicitly
```sh
IMAGE=$(docker ps | grep prom/prometheus | awk '{print $1}' )
docker cp prometheus.yml  $IMAGE:/etc/prometheus/prometheus.yml
```
if seeing the response
```text
Error response from daemon: Error processing tar file(exit status 1): unlinkat /etc/prometheus/prometheus.yml: device or resource busy
```
rerun without the volume option
* reload prometheus server
```sh
curl -X POST  http://localhost:9090/-/reload
```
confirm the configuration event in the logs
```text
level=info ts=2022-06-02T20:10:31.068Z caller=main.go:957 msg="Loading configuration file" filename=/etc/prometheus/prometheus.yml
level=info ts=2022-06-02T20:10:31.069Z caller=main.go:988 msg="Completed loading of configuration file" filename=/etc/prometheus/prometheus.yml totalDuration=1.046178ms remote_storage=4.542탎 web_handler=1.061탎 query_engine=2.172탎 scrape=257.815탎 scrape_sd=115.097탎 notify=2.14탎 notify_sd=2.483탎 rules=3.04탎
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

* query data from Prometheus directly using documented [REST Query API](https://prometheus.io/docs/prometheus/latest/querying/api/)

```sh
 curl -s "http://localhost:9090/api/v1/query?query=disk" | jq '.'
```

NOTE : no time argument was added in the above request

get response which will look like
```JSON
{
  "status": "success",
  "data": {
    "resultType": "vector",
    "result": [
      {
        "metric": {
          "__name__": "disk",
          "application": "application01",
          "datacenter": "dummy",
          "group": "application",
          "instance": "hostname00",
          "job": "application",
          "linborg_instance": "instance01"
        },
        "value": [
          1657156159.918,
          "40.51499938964844"
        ]
      },
      {
        "metric": {
          "__name__": "disk",
          "application": "application01",
          "datacenter": "dummy",
          "group": "application",
          "instance": "hostname00",
          "job": "application",
          "linborg_instance": "instance02"
        },
        "value": [
          1657156159.918,
          "40.59000015258789"
        ]
      },
      {
        "metric": {
          "__name__": "disk",
          "application": "application01",
          "datacenter": "dummy",
          "group": "application",
          "instance": "hostname01",
          "job": "application",
          "linborg_instance": "instance03"
        },
        "value": [
          1657156159.918,
          "41.56999969482422"
        ]
      },
      {
        "metric": {
          "__name__": "disk",
          "application": "application01",
          "datacenter": "dummy",
          "group": "application",
          "instance": "hostname01",
          "job": "application",
          "linborg_instance": "instance04"
        },
        "value": [
          1657156159.918,
          "41.518001556396484"
        ]
      },
...
// truncated the JSON
```

NOTE: the float data entry next to each value

```sh
curl -s "http://localhost:9090/api/v1/query?query=disk" | jq '.data.result[0].value[0]'
```
```text
1657156681.826
```

is the timestamp in second with millisecond precisision.

to prove, convert it  like one will do with epoch seconds to regular human calendar date format:

```sh
date --date="@1657156159"
Wed Jul  6 21:09:19 EDT 2022
```
one line command is
```sh
curl -s "http://localhost:9090/api/v1/query?query=disk" | jq '.data.result[0].value[0]' |sed 's|\.[0-9][0-9]*$||' |xargs -IX  date --date=@X
```
this will produce
```
Wed Jul  6 21:22:16 EDT 2022
```
providing the `time` and `range` argument to Prometheus query is a work in progress

### Historic data

Try to configure prometheus to ingest `http://application:8080/staticmetrics`
which are hard coded to timestamp of `Fri Jul 1 13:52:59 EDT 2022` by making relevant edits in `prometheus.yml` and recycle and relaunch `prometheus`
container linked to `application` one

observe in the `prometheus` logs

```text
level=warn ts=2022-07-07T21:21:33.712Z caller=scrape.go:1473 component="scrape manager"
scrape_pool=application
target=http://application:8080/staticmetrics msg="Error on ingesting samples that are too old or are too far into the future" num_dropped=5
```
repeat with `date -d "-1 hour"` - observe same error. Finally made the offset in minutes a query parameter of the `/pastmetrics/60` endpoint in `prometheus.yml`:
```YAML
scrape_configs:
  - job_name:       'application'

    scrape_interval: 10s
    metrics_path: /pastmetrics/60
    honor_labels: true

    static_configs:
      - targets: ['application:8080']
        labels:
          group: 'application'

```
the oldest succesfully ingested data point is 60 minute ago (61 is "too old"):
```text
level=warn ts=2022-07-07T23:08:59.701Z caller=scrape.go:1473 component="scrape manager"
scrape_pool=application target=http://application:8080/pastmetrics/61 msg="Error on ingesting samples that are too old or are too far into the future" num_dropped=5


```
![Sample Successfully Ingested Historic Datapoint](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture-historic-datapoint.png)
one appears to be able to successfully combine the instant and past metrics from two separate jobs (*not certain if merging into same job would pass the test*):
```yaml
```

![Sample with Two Targets](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture_instant_past.png)

![Sample Successfully Ingested Historic and Current Datapoint](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-counter/screenshots/capture_data_feeds_combined.png)

### Cleanup

```sh
docker container rm -f $NAME
docker container prune -f
docker image rm $IMAGE
docker image rm prom/prom2json
docker image rm prom/prom2json:v1.3.0
docker image rm prom/prometheus:v2.27.0
```
### `HostData` Class

* transforms legacy Java property-style metrics `data.txt`:
```text
cpu: 10
memory: 20
disk: 40.5
load_average: 1 2 3 4 6
rpm: 100
uptime: 0
```
into TimeSeries metric Prometheus inputs tagged with an array of labels
```text
```

configured to read the names of the labels from `application.properties` array
```java
example.labelNames = instance,dc,app,env
```
and of metrics from a combination of metric name array
```java
example.metricNames = memory,cpu,disk,load_average
```
and keys of `MetricExtracrors` map:
```java
example.metricExtractors = {'load_average':'\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*',rpm:'\\b(\\d+)\\b', rpm_custom_name:'\\b(\\d+)\\b'}
```
optionally renaming on the fly if the `example.extractedMetricNames` map is present in `applicatiion.properties`:
```java
example.extractedMetricNames = { 'load_average': 'cpu_load'}
```
### See Also

  * Docker hub link for Prometheus [prometheus](https://hub.docker.com/r/prom/prometheus)
  * Docker hub link for Prom2JSON [prom2json](https://hub.docker.com/r/prom/prom2json)
  * [Java examples for Prometheus](https://github.com/RobustPerception/java_examples)
  * [minimal prometheus counter example](https://github.com/njanor/spring-boot-prometheus-demo) - uses older Springboot and currently does not compile, integrated the `build status` counter from the example
  * [step by step springboot monitoring dashboard example](https://github.com/rishant/springboot-monitoring-dashboard-example)
  * [example](https://github.com/ramesh-lingappan/prometheus-pushgateway-demo) - demonstrates `PushGatewayConfiguration`  and `PushGatewayCredentials` bolierplate code
  * old [MVC example](https://github.com/ConSol/springboot-monitoring-example) with static page links to generated metrics, the application handles the `/metrics` route on its own.
  * Spring Boot metrics monitoring using Prometheus & Grafana [blog](https://aboullaite.me/spring-boot-monitoring-prometheus-grafana/) - relies on `@EnablePrometheusEndpoint` and `@EnableSpringBootMetricsCollector` annotations which do not work yet in this project
  * core *metrics Collection in Spring Boot With Micrometer and Prometheus* [documentation](https://www.codeprimers.com/metrics-collection-in-spring-boot-with-micrometer-and-prometheus/)
  * [collection](https://awesome-prometheus-alerts.grep.to) of alerting rules for Prometheus
  * plain Java (non-Spring) [metric push demo application](https://github.com/binhhq/prometheus-pushgateway-demo) pushing the data to prometheus [pushgateway](https://github.com/prometheus/pushgateway)
    via `PushGateway` [adapter class](https://prometheus.github.io/client_java/io/prometheus/client/exporter/PushGateway.html)
  * Prometheus for Java Developers Demo [project](https://github.com/fstab/prometheus-for-java-developers) - note there are severa subprojects there with an interesting per-project layout of git branches
  * https://www.tabnine.com/code/java/methods/io.prometheus.client.Collector$MetricFamilySamples$Sample/%3Cinit%3E
  * https://www.tabnine.com/code/java/methods/io.prometheus.client.CollectorRegistry/register
  * https://prometheus.github.io/client_java/io/prometheus/client/Collector.MetricFamilySamples.Sample.html
  * https://prometheus.github.io/client_java/io/prometheus/client/Collector.MetricFamilySamples.html
  * Prometheus [metric types](https://prometheus.io/docs/concepts/metric_types/)
  * [stackoverflow](https://stackoverflow.com/questions/26275736/how-to-pass-a-mapstring-string-with-application-properties) on defining `Map<String,String>` through `application.properties` and `@Value` annotation
  * [stackoverflow](https://stackoverflow.com/questions/6212898/spring-properties-file-get-element-as-an-array) on defining `Array<String>` through `application.properties` and `@Value` annotation
  * [tutorial](https://www.baeldung.com/spring-yaml-inject-map)	 on defining `Map<String,String>` through `application.yml` YAML and `@Value` annotation
  * Prometheus data query [REST API](https://prometheus.io/docs/prometheus/latest/querying/api/)

### See Also

  * Prometheus Java Graphite Bridge [example code](https://github.com/RobustPerception/java_examples/tree/master/java_graphite_bridge)
  * [about](https://www.robustperception.io/exporting-to-graphite-with-the-prometheus-java-client/) exporting to Prometheus [Graphite](https://graphite.readthedocs.io/en/latest/overview.html) with the Java client - mentions Prometheusonly indirectly as provider of the GraphiteBridge Python [package](https://github.com/stuart-c/prometheus-graphite-bridge)
  * Python GraphiteBridge [examples](https://python.hotexamples.com/examples/prometheus_client.bridge.graphite/GraphiteBridge/-/python-graphitebridge-class-examples.html)
  * [comparison](https://logz.io/blog/prometheus-vs-graphite) of Prometheus vs. Graphite - it appears Graphite is like InfluxDB
  * [comparison](https://www.loomsystems.com/blog/single-post/2017/06/07/prometheus-vs-grafana-vs-graphite-a-feature-comparison) of Prometheus vs. Grafana vs. Graphite

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
