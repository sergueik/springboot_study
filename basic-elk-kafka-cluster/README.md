### Usage

*  build app
```sh
for F in app1 app2; do pushd  $F; mvn package ;   popd; done
```
* build cluster 
```sh
docker-compose up --build -d
```

this will construct a minimal cluster running Java Spriung apps exchanging messages through Kafka.

> NOTE if you want to only rebuild `app1`, `app2` then
```sh
docker-compose stop app1 app2
docker-compose rm -f app1 app2
``` 
```sh
docker-compose ps
```

![Cluster](screenshots/capture-cluster.png)


```txt
   Name                   Command                  State                            Ports                     
---------------------------------------------------------------------------------------------------------------
apm-server      /usr/bin/tini -- /usr/loca ...   Up (healthy)   0.0.0.0:8200->8200/tcp,:::8200->8200/tcp       
app1            java -javaagent:/home/elas ...   Up (healthy)   0.0.0.0:8080->8080/tcp,:::8080->8080/tcp       
app2            java -javaagent:/home/elas ...   Up                                                            
elasticsearch   /bin/tini -- /usr/local/bi ...   Up (healthy)   0.0.0.0:9200->9200/tcp,:::9200->9200/tcp,      
                                                                9300/tcp                                       
kafka           /etc/confluent/docker/run        Up             0.0.0.0:9092->9092/tcp,:::9092->9092/tcp       
kibana          /bin/tini -- /usr/local/bi ...   Up (healthy)   0.0.0.0:5601->5601/tcp,:::5601->5601/tcp  
```
Both services are known to Elastic

![Service Visibility](screenshots/capture-kibana-observability-apm-services.png)


* Start the transaction

```sh
curl -sv -XPOST "http://localhost:8080/basic/publish?topic=demo-topic" -H 'Content-Type: application/json' -d '{"name": "new value"}'
```
```text
published 21 to demo-topic
```
>NOTE:  currently only one topic `demo-topic` is known to `app2` and is har oded.

* confirm the payload was posted to Kafka
```sh
docker-compose logs app1
```
```text
app1             | 2026-03-30 21:42:50.221  INFO 1 --- [io-8080-exec-10] o.a.kafka.common.utils.AppInfoParser     : Kafka commitId: f8c67dc3ae0a3265
app1             | 2026-03-30 21:42:50.221  INFO 1 --- [io-8080-exec-10] o.a.kafka.common.utils.AppInfoParser     : Kafka startTimeMs: 1774906970218
app1             | 2026-03-30 21:42:50.799  INFO 1 --- [ad | producer-1] org.apache.kafka.clients.Metadata        : [Producer clientId=producer-1] Resetting the last seen epoch of partition demo-topic-0 to 4 since the associated topicId changed from null to GuyR47BRTwmatE3NTgxTJQ
app1             | 2026-03-30 21:42:50.803  INFO 1 --- [ad | producer-1] org.apache.kafka.clients.Metadata        : [Producer clientId=producer-1] Cluster ID: MkU3OEVBNTcwNTJENDM2Qg
app1             | 2026-03-30 21:42:50.811  INFO 1 --- [ad | producer-1] o.a.k.c.p.internals.TransactionManager   : [Producer clientId=producer-1] ProducerId set to 1002 with epoch 0
app1             | published 21 to demo-topic
```
* confirm the payload is present on Kafka

```sh
docker-compose exec kafka kafka-topics --bootstrap-server localhost:9092 --list
```

```text
__consumer_offsets
demo-topic
your-topic
```

```sh
docker-compose exec kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic demo-topic --from-beginning
```
abort by `^C`
```text
{"name": "value"}
{"name": "new value"}
^CProcessed a total of 2 messages
```
* confirm the payload is colleted by `app2` from Kafka

```sh
docker-compose logs app2 
```
```text
docker-compose logs app2 
app2             | received: {"name": "new value"}
```

Examine the Elastic APM Trace context propagation over Kafka find the operation that starts it

![Traces Visibility](screenshots/capture-kibana-observability-apm-traces.png)


One can see the trace in action by drilling into waterfall steps 

in :

-----|------------------- | ---------------------
role | property           |    value 
-----|------------------- | ---------------------
__app1__ | `span.id`             | `f89f900d2ef87c5a`	
__app2__  | `parent.id`          | `f89f900d2ef87c5a`


There is also a flow fragments:

![Fragment1](screenshots/capture-flow-fragment1.png)

![Fragment2](screenshots/capture-flow-fragment2.png)


### Technical Details
```sh
docker-compose run kafka kafka-console-consumer --bootstrap-server localhost:9092 --topic demo-topic --from-beginning --property print.headers=true
```
```text
traceparent:00-602de1168690be4a4eeccb1290a10092-04e9f6c9463d8fb8-01,elasticapmtraceparent:`-����JN��������F=��,tracestate:es=s:1	{"name": "new value"}
```
also can print headers in the `app2`:
```text
app2             | received: {"name": "new value"}
app2             | traceparent = 00-cc112640a8c24a15f1b18ca6818408fd-c0e9ee0231d14d2e-01
app2             | elasticapmtraceparent = �&@��J񱌦�����1�M.
app2             | tracestate = es=s:1
```

### Cleanup

```sh
docker-compose stop ; docker-compose rm -f 

docker image prune -f
docker container prune -f
docker image ls | grep -Ei '^basic-elk-kafka-cluster' | awk '{print $1'} | xargs -IX   docker image rm X
ocker image rm "docker.elastic.co/apm/apm-server:8.17.8" "confluentinc/cp-kafka:7.6.0" "elasticsearch:8.17.8" "kibana:8.17.8"
docker volume prune -f
```
### Background

Elasticsearch tracks asynchronous tracing in Kafka-based systems using distributed tracing, typically via Elastic APM, to link decoupled producers and consumers. Interceptors propagate trace metadata through Kafka headers, allowing end-to-end visibility of message flows. This enables monitoring of Kafka lag, latency, and message lifecycles in Kibana dashboards

> NOTE: To  engage disctibuted tracing one instruments the producer + consumer apps, not the Kafka broker container
Kafka itself is just transporting bytes. The trace context lives in message headers, not inside the broker runtime.

### See Also

  * [observability in Distributed Systems](https://www.baeldung.com/distributed-systems-observability)
  * [how to monitor containerized Kafka with Elastic Observability](https://www.elastic.co/blog/how-to-monitor-containerized-kafka-with-elastic-observability)
  * [ELK Kafka Integration](https://www.elastic.co/docs/reference/integrations/kafka) 	
  * [micrometer Observation and Spring Kafka](https://www.baeldung.com/spring-kafka-micrometer)
  * [structured logging in Spring Boot 3.4](https://spring.io/blog/2024/08/23/structured-logging-in-spring-boot-3-4)
---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
