### Info

This project is a replica of one in [Enable Elastic APM in Spring Boot Application](https://bhuwanupadhyay.github.io/posts/enable-elastic-apm-in-spring-boot-application) but without relying on docker-compose and 
with a different app to get instrumented by [elasticsearch APM agent]()

### Usage
Note: in elasticsearch stack standalone node run seems to be yet found. Try to fire several application docker process
```sh
ELASTICSEARCH_BASE_IMAGE=docker.elastic.co/elasticsearch/elasticsearch:6.4.2
docker pull $ELASTICSEARCH_BASE_IMAGE
```
```sh
ELASTICSEARCH_SERVER=apm-elasticsearch
docker run --name $ELASTICSEARCH_SERVER -d $ELASTICSEARCH_BASE_IMAGE
```
```sh
BASE_IMAGE=docker.elastic.co/apm/apm-server:6.4.2
docker pull $BASE_IMAGE 
```
and run it with environments matching the `application.properties`:
```sh
APM_SERVER=elastic-apm-server 
docker run --name $APM_SERVER -d $BASE_IMAGE 
```
```sh
docker build -f Dockerfile.app -t app_server .
```
* Lanch the `mysql-example` backed Docker container
```sh
docker run -p 8086:8085 -e ELASTIC_APM_SERVICE_NAME=app_server -e ELASTIC_APM_APPLICATION_PACKAGES=example.basic -e ELASTIC_APM_SERVER_URLS=http://$APM_SERVER:8200 --link $APM_SERVER -d app_server
```
 will also need the elasticsearch running: the app_server loops with
```sh
	Failed to connect to backoff(elasticsearch(http://elasticsearch:9200)): Get http://elasticsearch:9200: lookup elasticsearch on 75.75.75.75:53: no such host
2021-03-03T22:38:04.850Z	INFO	pipeline/output.go:93	Attempting to reconnect to backoff(elasticsearch(http://elasticsearch:9200)) with 98 reconnect attempt(s)
2021-03-03T22:38:04.879Z	WARN	transport/tcp.go:53	DNS lookup failure "elasticsearch": lookup elasticsearch on 75.75.75.75:53: no such host
```sh

and this is problematic on a low  resource laptop:
 the elasticsearch docker image fails with reporting insufficient resources:
```sh
[2021-03-04T00:17:15,536][INFO ][o.e.t.TransportService   ] [N4XWO8j] publish_address {172.18.0.2:9300}, bound_addresses {0.0.0.0:9300}
apm-elasticsearch_1   | [2021-03-04T00:17:15,579][INFO ][o.e.b.BootstrapChecks    ] [N4XWO8j] bound or publishing to a non-loopback address, enforcing bootstrap checks
apm-elasticsearch_1   | ERROR: [1] bootstrap checks failed
apm-elasticsearch_1   | [1]: max virtual memory areas vm.max_map_count [65530] is too low, increase to at least [262144]

```

### See Also

https://github.com/elastic/apm-agent-java/blob/master/CONTRIBUTING.md
docker.elastic.co/observability/apm-agent-java:1.12.
https://mvnrepository.com/artifact/co.elastic.apm/elastic-apm-agent
https://www.elastic.co/guide/en/apm/get-started/current/install-and-run.html
https://www.elastic.co/guide/en/apm/server/6.8/running-on-docker.html
https://www.docker.elastic.co/r/apm/apm-server
https://www.elastic.co/guide/en/apm/server/6.8/running-on-docker.html
https://www.elastic.co/guide/en/apm/server/current/running-on-docker.html

https://stackoverflow.com/questions/11570132/generator-functions-equivalent-in-java

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
