### Info

This project is a replica of one in [Enable Elastic APM in Spring Boot Application](https://bhuwanupadhyay.github.io/posts/enable-elastic-apm-in-spring-boot-application) but without relying on docker-compose and
with a different app to get instrumented by [elasticsearch APM agent](https://www.elastic.co/guide/en/apm/agent/java/1.x/supported-technologies-details.html)

### Usage

Note: [elasticsearch stack standalone Vagrantbox packaged by Bitnami](https://bitnami.com/stack/elk)
is a good alternative.


* try to fire several application docker process serially

```sh
ELASTICSEARCH_BASE_IMAGE=blacktop/elasticsearch
docker pull $ELASTICSEARCH_BASE_IMAGE
```
```sh
ELASTICSEARCH_SERVER=apm-elasticsearch
docker run --name $ELASTICSEARCH_SERVER -p 9200:9200 -p 9300:9300 -d $ELASTICSEARCH_BASE_IMAGE
```
followed by
```sh
docker logs $ELASTICSEARCH_SERVER
```
```sh
APM_SERVER_BASE_IMAGE=docker.elastic.co/apm/apm-server:7.8.0
docker pull $APM_SERVER_BASE_IMAGE
```
and run it with environments matching the `application.properties`:
```sh
APM_SERVER=elastic-apm-server
docker run --name $APM_SERVER --link $ELASTICSEARCH_SERVER -d $APM_SERVER_BASE_IMAGE
```
Note:  version __0.7.0__ may not be the latest version compatible with __APM Server 6.4.2__.

```sh
ELASTIC_APM_AGENT_VERSION=0.7.0
wget https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar \-O elastic-apm-agent.jar

docker build -f Dockerfile.app -t app_server .
```
* Lanch the `mysql-example` backed Docker container, using [Environment variables configuration]() option supported by APM Agent
```sh
docker run -d -p 8086:8085 -e ELASTIC_APM_SERVICE_NAME=app_server -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://$APM_SERVER:8200 --link $APM_SERVER app_server
```

you may try to install the APM agent into the App container
in which case the following argument update will be required:

```sh
docker run -d -p 8086:8085 -e ELASTIC_APM_SERVICE_NAME=app_server -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://localhost:8200 --link $ELASTICSEARCH_SERVER app_server
```
 will also need link to the elasticsearch running: the app_server loops with
```sh
Failed to connect to backoff(elasticsearch(http://elasticsearch:9200)): Get http://elasticsearch:9200: lookup elasticsearch on 75.75.75.75:53: no such host
2021-03-03T22:38:04.850Z	INFO	pipeline/output.go:93	Attempting to reconnect to backoff(elasticsearch(http://elasticsearch:9200)) with 98 reconnect attempt(s)
2021-03-03T22:38:04.879Z	WARN	transport/tcp.go:53	DNS lookup failure "elasticsearch": lookup elasticsearch on 75.75.75.75:53: no such host
```sh

and this is problematic on a insufficient resources on the developer laptop:
 the elasticsearch docker image fails with reporting insufficient resources:
```sh
[2021-03-04T00:17:15,536][INFO ][o.e.t.TransportService   ] [N4XWO8j] publish_address {172.18.0.2:9300}, bound_addresses {0.0.0.0:9300}
apm-elasticsearch_1   | [2021-03-04T00:17:15,579][INFO ][o.e.b.BootstrapChecks    ] [N4XWO8j] bound or publishing to a non-loopback address, enforcing bootstrap checks
apm-elasticsearch_1   | ERROR: [1] bootstrap checks failed
apm-elasticsearch_1   | [1]: max virtual memory areas vm.max_map_count [65530] is too low, increase to at least [262144]

```
after the successful launch via `docker-compose` and some interaction with the app server performed in the browser 
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-apm/screenshots/capture_application.png)

one can observe the activy posting data to `apm-server` in the `app` server console logs:

```sh
APP_SERVER_ID=$(docker container ls -a | grep 'app-server' | awk '{print $1}' )
```
```sh
docker logs $APP_SERVER_ID |  grep apm-server
```
```text
2021-04-08 00:13:39.703 [apm-server-healthcheck] WARN co.elastic.apm.report.ApmServerHealthChecker - Elastic APM server is not available (404)
2021-04-08 00:13:39.810 [main] INFO co.elastic.apm.configuration.StartupInfo - Starting Elastic APM 0.7.0 as app-server on Java 1.8.0_212 (IcedTea) Linux 5.4.0-42-generic
2021-04-08 00:16:31.774 [OkHttp ConnectionPool] WARN co.elastic.apm.shaded.okhttp3.OkHttpClient - A connection to http://elastic-apm-server:8200/ was leaked. Did you forget to close a response body? To see where this was allocated, set the OkHttpClient logger level to FINE: Logger.getLogger(OkHttpClient.class.getName()).setLevel(Level.FINE);
```
in the current environment cannot yet browse the data in kibana:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-apm/screenshots/capture_kibana.png)
 
probably due to some licensing problem, kibana itself loads but refuses to show elasticsearch.

### Speed up building ELK docker container image 

it helps caching the binaries in the workspace
```sh
VERSION=7.2.0
ES_URL="https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-${VERSION}-linux-x86_64.tar.gz"
LS_URL="https://artifacts.elastic.co/downloads/logstash/logstash-${VERSION}.tar.gz"
K_URL="https://artifacts.elastic.co/downloads/kibana/kibana-${VERSION}-linux-x86_64.tar.gz"
```
```sh
wget -q $ES_URL -O elasticsearch.tar.gz 
wget -q $LS_URL -O logstash.tar.gz 
wget -q $K_URL -O kibana.tar.gz
```
and replace the RUN commands in `Dockerfile.elk` with the COPY commands 
### See Also

  * https://github.com/elastic/apm-agent-java/blob/master/CONTRIBUTING.md
  * docker.elastic.co/observability/apm-agent-java:1.12.
  * https://mvnrepository.com/artifact/co.elastic.apm/elastic-apm-agent
  * https://www.elastic.co/guide/en/apm/get-started/current/install-and-run.html
  * https://www.docker.elastic.co/r/apm/apm-server
  * https://www.elastic.co/guide/en/apm/server/6.8/running-on-docker.html
  * https://www.elastic.co/guide/en/apm/server/current/running-on-docker.html
  * https://stackoverflow.com/questions/11570132/generator-functions-equivalent-in-java
  * https://stackoverflow.com/questions/51445846/elasticsearch-max-virtual-memory-areas-vm-max-map-count-65530-is-too-low-inc
  * [blacktop/docker-elasticsearch-alpine](https://github.com/blacktop/docker-elasticsearch-alpine/blob/master/6.4/Dockerfile)
  * [blacktop/docker-elastic-stack](https://github.com/blacktop/docker-elastic-stack/blob/master/docker-compose.yml) based on Alpine
  * [Monitor Spring Boot Application Performance with Elastic APM, Elasticsearch and Kibana](https://github.com/cosminseceleanu/tutorials/blob/master/elastic-apm-java/docs/index.md) Docker-hosted APM agent / server scenario 
  * Elasticsearch, Logstash and Kibana 7.x single [image](https://github.com/githubcdr/docker-elk7)
  * https://slacker.ro/2020/09/02/monitoring-java-applications-with-elastic-getting-started-with-the-elastic-apm-java-agent/
  * interactively download apm jar [link](https://search.maven.org/artifact/co.elastic.apm/elastic-apm-agent/1.20.0/jar)
  * https://discuss.elastic.co/t/will-there-eventually-be-alpine-based-docker-images-for-logstash-6-x/163623/4
  * [review of](https://www.elastic.co/blog/elasticsearch-as-a-time-series-data-store) __Elasticsearch as a Time Series Data Store__
  * https://www.elastic.co/guide/en/elasticsearch/reference/current/use-elasticsearch-for-time-series-data.html
  * [7 ways to ingest data into Elasticsearch](https://aravind.dev/elastic-data-ingest/)
  * C# elastic searh metric  gauge ingesting [client](https://github.com/Streets-Heaver/ElasticSeries) (needs VS 2015+, uses async/await)
  * java [Elasticsearch Rest Client](https://github.com/searchbox-io/Jest) - NOTE, uses modules within the project. The tool documentation is in [jest](https://github.com/searchbox-io/Jest/tree/master/jest)
  * [spring client](https://github.com/spring-projects/spring-data-elasticsearch) using `RestHighLevelCLient`
  * https://github.com/dadoonet/fscrawler comlex media crawler for ELK - may have some useful common code, but not specifically for TDSB scenario
  * sql4es [github repository](https://github.com/Anchormen/sql4es) and [release direcory](https://github.com/Anchormen/sql4es/tree/master/release) Elastic adapter JDBC.  Note - not published to Maven central. There s no newer version than for elastic search 6.3.2 4 year ago
  * https://github.com/wilsonyy/spring-boot-elasticsearch-timeseries-demo - create and query time series, interesting
  * https://github.com/ajaypp123/elasticsearch_timeseries/blob/master/timeseries_python.py
 
###  Youtube Links
  * [Configuring Elasticsearch Index for Time Series Data](https://www.youtube.com/watch?v=2WJFMYAri_8)
  * [time series sata with Index Lifecycle Management (ILM) policies](https://www.youtube.com/watch?v=6oEtbyrByRk)
  * [Using Elasticsearch as a Time-Series Database ](https://www.youtube.com/watch?v=hNzxm157gPg)
  * [load data into ElasticSearch index using LogStash](https://www.youtube.com/watch?v=hIDIv4-CElc)
  * [Benchmarking Elasticsearch vs InfluxDB for Time Series Data & Metrics](https://www.youtube.com/watch?v=qeg2jwpWhPU)
  * [How to load data into ElasticSearch index using LogStash](https://www.youtube.com/watch?v=hIDIv4-CElc)
  * [How to Use Logstash to import CSV Files Into ElasticSearch](https://www.youtube.com/watch?v=_kqunm8w7GI)
  * [Configuring Elasticsearch Index for Time Series Data](https://www.youtube.com/watch?v=2WJFMYAri_8)
  * [overview of TSDB incl. Elastic](https://youtu.be/HB9bG3Qcvq8?t=365)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
