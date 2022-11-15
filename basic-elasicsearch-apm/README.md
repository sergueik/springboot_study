### Info

This project is a replica of one in [Enable Elastic APM in Spring Boot Application](https://bhuwanupadhyay.github.io/posts/enable-elastic-apm-in-spring-boot-application)
but without relying on `docker-compose` and
with a different app to get instrumented by [elasticsearch APM agent](https://www.elastic.co/guide/en/apm/agent/java/1.x/supported-technologies-details.html)

### Usage

Note: [elasticsearch stack standalone Vagrantbox packaged by Bitnami](https://bitnami.com/stack/elk)
is a good alternative.


* try to fire several application docker process serially

```sh
ELASTICSEARCH_BASE_IMAGE=blacktop/elasticsearch
docker pull $ELASTICSEARCH_BASE_IMAGE
```

NOTE: may need to pin the older version

run Elastic Search server in container publishing the default ports to the developer host
```sh
ELASTICSEARCH_SERVER=apm-elasticsearch
docker run --name $ELASTICSEARCH_SERVER -p 9200:9200 -p 9300:9300 -d $ELASTICSEARCH_BASE_IMAGE
```
followed by log check
```sh
docker logs $ELASTICSEARCH_SERVER
```

to see success messges e.g.

```text
[2022-11-15T00:33:12,228][INFO ][o.e.c.r.a.AllocationService] [ff10feba7f4b] current.health="GREEN" message="Cluster health status changed from [YELLOW] to [GREEN] (reason: [shards started [[.geoip_databases][0]]])." previous.health="YELLOW" reason="shards started [[.geoip_databases][0]]"
```
confirm with REST API  check

```sh
curl -s http://localhost:9200/
```
```json
{
  "name" : "ff10feba7f4b",
  "cluster_name" : "docker-cluster",
  "cluster_uuid" : "-LdyhR1QQ2-uA3AV20cbsA",
  "version" : {
    "number" : "8.1.2",
    "build_flavor" : "default",
    "build_type" : "tar",
    "build_hash" : "31df9689e80bad366ac20176aa7f2371ea5eb4c1",
    "build_date" : "2022-03-29T21:18:59.991429448Z",
    "build_snapshot" : false,
    "lucene_version" : "9.0.0",
    "minimum_wire_compatibility_version" : "7.17.0",
    "minimum_index_compatibility_version" : "7.0.0"
  },
  "tagline" : "You Know, for Search"
}

```
pull the pinned version of Elastic APM Server (not the  latest)
```sh
APM_SERVER_BASE_IMAGE=docker.elastic.co/apm/apm-server:7.8.0
docker pull $APM_SERVER_BASE_IMAGE
```
and run it with environments matching the `application.properties`:
```sh
APM_SERVER=elastic-apm-server
docker run --name $APM_SERVER --link $ELASTICSEARCH_SERVER -d $APM_SERVE  R_BASE_IMAGE
```

connect into `$APM_SERVER` to confirm connectivity

```sh
docker exec -it $APM_SERVER sh
```

```sh
ping -c 1 apm-elasticsearch
```
```text
PING apm-elasticsearch (172.17.0.2) 56(84) bytes of data.
64 bytes from apm-elasticsearch (172.17.0.2): icmp_seq=1 ttl=64 time=0.079 ms

```
```sh
curl -s http://apm-elasticsearch:9200
```
```json
{
  "name" : "ff10feba7f4b",
  "cluster_name" : "docker-cluster",
  "cluster_uuid" : "-LdyhR1QQ2-uA3AV20cbsA",
  "version" : {
    "number" : "8.1.2",
    "build_flavor" : "default",
    "build_type" : "tar",
    "build_hash" : "31df9689e80bad366ac20176aa7f2371ea5eb4c1",
    "build_date" : "2022-03-29T21:18:59.991429448Z",
    "build_snapshot" : false,
    "lucene_version" : "9.0.0",
    "minimum_wire_compatibility_version" : "7.17.0",
    "minimum_index_compatibility_version" : "7.0.0"
  },
  "tagline" : "You Know, for Search"
}

```
Note:  version __0.7.0__ may not be the latest version compatible with __APM Server 6.4.2__.

* package the app

```sh
mvn package
```
```sh
ELASTIC_APM_AGENT_VERSION=1.30.0
wget https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar \-O elastic-apm-agent.jar
APP_SERVER=app_server

docker build -f Dockerfile.app -t $APP_SERVER .
```
NOTE: may like to update the 

```text
ARG ELASTIC_APM_AGENT_VERSION="1.30.0"
```

in the `Dockerfile.app`

* Lanch the `mysql-example` backed Docker container, using [Environment variables configuration]() option supported by APM Agent
publish TCP port `8080`
```sh
APP_SERVER=app_server
docker run --name $APP_SERVER -d -p 8080:8080 -e ELASTIC_APM_SERVICE_NAME=$APP_SERVER -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://$APM_SERVER:8200 --link $APM_SERVER $APP_SERVER
```

check logs of the spring app:
```text
2022-11-15 00:47:45.307  INFO 1 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat started on port(s): 8080 (http)
2022-11-15 00:47:45.315  INFO 1 --- [           main] example.Launcher                         : Started Launcher in 7.859 seconds (JVM running for 19.851)
```
you may try to install the APM server into the App container, too
in which case the following argument update will be required:

```sh
docker run -d -p 8086:8085 -e ELASTIC_APM_SERVICE_NAME=$APP_SERVER -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://localhost:8200 --link $ELASTICSEARCH_SERVER $APP_SERVER
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
after the successful launch via `docker-compose` and some interaction with the app server performed through the browser
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-apm/screenshots/capture_application.png)

one can observe the activy posting data to `apm-server` in the `app` server console logs:

```sh
APP_SERVER_ID=$(docker container ls -a | grep 'app-server' | awk '{print $1}' )
```
```sh
docker logs $APP_SERVER_ID | grep apm-server
```
or simply
```sh
docker logs $APP_SERVER | grep apm-server
```

```text
2022-11-15 01:06:32,777 [main] INFO  co.elastic.apm.agent.configuration.StartupInfo - server_urls: 'http://elastic-apm-server:8200' (source: Environment Variables)
2022-11-15 01:06:42,133 [elastic-apm-server-healthcheck] INFO  co.elastic.apm.agent.report.ApmServerHealthChecker - Elastic APM server is available: 
{
  "build_date": "2020-06-14T17:10:16Z",
  "build_sha": "06c58bf4e5b675d04314bf44961ffd6b0e13f544",
  "version": "7.8.0"
}
```
in the current environment cannot yet browse the data in kibana:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-apm/screenshots/capture_kibana.png)
 
probably due to some licensing problem, kibana itself loads but refuses to show elasticsearch.

### Cleanup
```sh
docker container stop $APP_SERVER
docker container stop $APM_SERVER
docker container stop $ELASTICSEARCH_SERVER

docker container rm $APP_SERVER
docker container rm $APM_SERVER
docker container rm $ELASTICSEARCH_SERVER
```
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

### Virtual Box hosted Elastic Search

* bring up the VM and log in as `vagrant`/`vagrant`. Find out the ip address on bind network

* run healthcheck in console

```sh
curl http://192.168.0.138:9200
```
```json
{
  "name" : "elasticsearch",
  "cluster_name" : "elasticsearch",
  "cluster_uuid" : "FtKAQ6YeQ_KYz9-AYGy2yg",
  "version" : {
    "number" : "7.3.1",
    "build_flavor" : "default",
    "build_type" : "rpm",
    "build_hash" : "4749ba6",
    "build_date" : "2019-08-19T20:19:25.651794Z",
    "build_snapshot" : false,
    "lucene_version" : "8.1.0",
    "minimum_wire_compatibility_version" : "6.8.0",
    "minimum_index_compatibility_version" : "6.0.0-beta1"
  },
  "tagline" : "You Know, for Search"
}

```

* download relatively recent version of the APM Agent jar
```sh
ELASTIC_APM_AGENT_VERSION=1.30.0
wget https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar \-O elastic-apm-agent.jar
```
* build image using a specific Dockerfile:

```sh
docker build --build-arg ELASTIC_APM_AGENT_VERSION=$ELASTIC_APM_AGENT_VERSION -f Dockerfile.app-vagrant -t app_server .
```
* Lanch the `mysql-example` backed Docker container, using [Environment variables configuration]() option supported by APM Agent
```sh
ELASTICSEARCH_SERVER_VM_IP=192.168.0.138
docker run -d -p 8086:8085 -e ELASTIC_APM_SERVICE_NAME=app_server -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://$ELASTICSEARCH_SERVER_VM_IP:9200 app_server
```
* check logs

```text
info - application_packages: 'example.basic' (source: Environment Variables)
2022-11-14 21:15:24,581 [main INFO  co.elastic.apm.agent.impl.ElasticApmTracer - Tracer switched to RUNNING state
2022-11-14 21:15:24,995 [elastic-apm-server-healthcheck] INFO  co.elastic.apm.agent.report.ApmServerHealthChecker - Elastic APM server is available: {  "name" : "elasticsearch",  "cluster_name" : "elasticsearch",  "cluster_uuid" : "FtKAQ6YeQ_KYz9-AYGy2yg",  "version" : {    "number" : "7.3.1",    "build_flavor" : "default",    "build_type" : "rpm",    "build_hash" : "4749ba6",    "build_date" : "2019-08-19T20:19:25.651794Z",    "build_snapshot" : false,    "lucene_version" : "8.1.0",    "minimum_wire_compatibility_version" : "6.8.0",    "minimum_index_compatibility_version" : "6.0.0-beta1"  },  "tagline" : "You Know, for Search"}
2022-11-14 21:15:25,003 [elastic-apm-server-healthcheck] WARN  co.elastic.apm.agent.report.ApmServerHealthChecker - Failed to parse version of APM server http://192.168.0.138:9200/: null
```
- need to reconfigure to use Elastic Search directly

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
