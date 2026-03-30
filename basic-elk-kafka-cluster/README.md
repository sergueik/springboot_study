### Troubleshooting

```text
RUN apt-get update && apt-get install -y wget curl && rm -rf /var/lib/apt/lists/*: #0 2.890 Get:1 http://archive.ubuntu.com/ubuntu noble InRelease [256 kB] #0 2.896 Get:2 http://security.ubuntu.com/ubuntu noble-security InRelease [126 kB] #0 3.904 Err:2 http://security.ubuntu.com/ubuntu noble-security InRelease #0 3.905 The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C #0 4.268 Get:3 http://archive.ubuntu.com/ubuntu noble-updates InRelease [126 kB] #0 4.276 Err:1 http://archive.ubuntu.com/ubuntu noble InRelease #0 4.277 The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
```
attempt to fix
```text
RUN apt-get install -y gnupg \
 && gpg --keyserver keyserver.ubuntu.com --recv-keys 871920D1991BC93C \
 && gpg --export 871920D1991BC93C | apt-key add -
```
fails with
```
RUN apt-get install -y gnupg && gpg --keyserver keyserver.ubuntu.com --recv-keys 871920D1991BC93C && gpg --export 871920D1991BC93C | apt-key add -: #0 0.207 Reading package lists... #0 0.222 Building dependency tree... #0 0.225 Reading state information... #0 0.237 gnupg is already the newest version (2.4.4-2ubuntu17.4). #0 0.237 0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded. #0 0.242 gpg: directory '/home/.gnupg' created #0 0.244 gpg: keybox '/home/.gnupg/pubring.kbx' created #0 6.284 gpg: can't connect to the dirmngr: End of file #0 6.284 gpg: keyserver receive failed: No dirmngr
```
switch error
```text
error with this image update: > [basic-elk-kafka-cluster-apm-server 2/9] 
RUN apt-get update: #0 0.832 Get:1 http://archive.ubuntu.com/ubuntu jammy InRelease [270 kB] #0 0.935 
Get:2 http://security.ubuntu.com/ubuntu jammy-security InRelease [129 kB] #0 1.636 
Get:3 http://archive.ubuntu.com/ubuntu jammy-updates InRelease [128 kB] #0 1.838 
Get:4 http://archive.ubuntu.com/ubuntu jammy-backports InRelease [127 kB] #0 2.076 
Err:1 http://archive.ubuntu.com/ubuntu jammy InRelease #0 2.078 
The following signatures couldn't be verified because the public key is not available: 
NO_PUBKEY 871920D1991BC93C #0 2.600 
Err:2 http://security.ubuntu.com/ubuntu jammy-security InRelease #0 2.603 
The following signatures couldn't be verified because the public key is not available: 
NO_PUBKEY 871920D1991BC93C
```
```
CONTAINER ID   IMAGE                                   COMMAND                  CREATED       STATUS                        PORTS                                                   NAMES
6ac7dbc06b8b   basic-elk-kafka-cluster_kibana          "/bin/tini -- /usr/l…"   6 hours ago   Restarting (1) 1 second ago                                                           kibana
a54999ab9326   confluentinc/cp-kafka:7.6.0             "/etc/confluent/dock…"   6 hours ago   Up 6 hours                    0.0.0.0:9092->9092/tcp, [::]:9092->9092/tcp             kafka
95327b999103   basic-elk-kafka-cluster_elasticsearch   "/bin/tini -- /usr/l…"   6 hours ago   Up 6 hours (healthy)          0.0.0.0:9200->9200/tcp, [::]:9200->9200/tcp, 9300/tcp   elasticsearch
```
```text
[.kibana_analytics] Action failed with '[index_not_green_timeout] Timeout waiting for the status of the [.kibana_analytics_8.17.8_001] index to become 'green' Refer to https://www.elastic.co/guide/en/kibana/8.17/resolve-migrations-failures.html#_repeated_time_out_requests_that_eventually_fail for information on how to resolve the issue.'. Retrying attempt 3 in 8 seconds.
kibana           | [2026-03-30T11:36:06.752+00:00][INFO ][savedobjects-service] [.kibana_analytics] CREATE_NEW_TARGET -> CREATE_NEW_TARGET. took: 304014ms.
kibana           | [2026-03-30T11:36:06.753+00:00][ERROR][savedobjects-service] [.kibana] Action failed with '[index_not_green_timeout] Timeout waiting for the status of the [.kibana_8.17.8_001] index to become 'green' Refer to https://www.elastic.co/guide/en/kibana/8.17/resolve-migrations-failures.html#_repeated_time_out_requests_that_eventually_fail for information on how to resolve the issue.'. Retrying attempt 3 in 8 seconds.
kibana           | [2026-03-30T11:36:06.753+00:00][INFO ][savedobjects-service] [.kibana] CREATE_NEW_TARGET -> CREATE_NEW_TARGET. took: 304013ms.
kibana           | [2026-03-30T11:36:06.754+00:00][ERROR][savedobjects-service] [.kibana_ingest] Action failed with '[index_not_green_timeout] Timeout waiting for the status of the [.kibana_ingest_8.17.8_001] index to become 'green' Refer to https://www.elastic.co/guide/en/kibana/8.17/resolve-migrations-failures.html#_repeated_time_out_requests_that_eventually_fail for information on how to resolve the issue.'. Retrying attempt 3 in 8 seconds.
kibana           | [2026-03-30T11:36:06.755+00:00][INFO ][savedobjects-service] [.kibana_ingest] CREATE_NEW_TARGET -> CREATE_NEW_TARGET. took: 304013ms.
kibana           | [2026-03-30T11:36:06.827+00:00][ERROR][savedobjects-service] [.kibana_usage_counters] Action failed with '[index_not_green_timeout] Timeout waiting for the status of the [.kibana_usage_counters_8.17.8_001] index to become 'green' Refer to https://www.elastic.co/guide/en/kibana/8.17/resolve-migrations-failures.html#_repeated_time_out_requests_that_eventually_fail for information on how to resolve the issue.'. Retrying attempt 3 in 8 seconds.
kibana           | [2026-03-30T11:36:06.827+00:00][INFO ][savedobjects-service] [.kibana_usage_counters] CREATE_NEW_TARGET -> CREATE_NEW_TARGET. took: 304014ms.
kibana           | [2026-03-30T11:36:06.837+00:00][ERROR][savedobjects-service] [.kibana_alerting_cases] Action failed with '[index_not_green_timeout] Timeout waiting for the status of the [.kibana_alerting_cases_8.17.8_001] index to become 'green' Refer to https://www.elastic.co/guide/en/kibana/8.17/resolve-migrations-failures.html#_repeated_time_out_requests_that_eventually_fail for information on how to resolve the issue.'. Retrying attempt 3 in 8 seconds.
kibana           | [2026-03-30T11:36:06.838+00:00][INFO ][savedobjects-service] [.kibana_alerting_cases] CREATE_NEW_TARGET -> CREATE_NEW_TARGET. took: 304017ms
```
TODO: disable  heavy plugins e.g.
```text
kibana           | [2026-03-30T11:45:08.979+00:00][INFO ][plugins.screenshotting.chromium] Browser executable: /usr/share/kibana/node_modules/@kbn/screenshotting-plugin/chromium/headless_shell-linux_x64/headless_shell
```


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
```sh
curl -X POST "http://localhost:9200/testindex/_doc/1" -H "Content-Type: application/json" -d '{"foo":"bar"}'
```
```sh
curl -X GET http://localhost:9200/_cluster/health?pretty
```
```json
{
  "cluster_name" : "docker-cluster",
  "status" : "red",
  "timed_out" : false,
  "number_of_nodes" : 1,
  "number_of_data_nodes" : 1,
  "active_primary_shards" : 0,
  "active_shards" : 0,
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 19,
  "unassigned_primary_shards" : 15,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 0.0
}
```
```sh
curl http://localhost:9200/_cluster/allocation/explain?pretty
```
```json
{
  "note" : "No shard was specified in the explain API request, so this response explains a randomly chosen unassigned shard. There may be other unassigned shards in this cluster which cannot be assigned for different reasons. It may not be possible to assign this shard until one of the other shards is assigned correctly. To explain the allocation of other shards (whether assigned or unassigned) you must specify the target shard in the request to this API. See https://www.elastic.co/guide/en/elasticsearch/reference/8.17/cluster-allocation-explain.html for more information.",
  "index" : ".kibana_ingest_8.17.8_001",
  "shard" : 0,
  "primary" : true,
  "current_state" : "unassigned",
  "unassigned_info" : {
    "reason" : "INDEX_CREATED",
    "at" : "2026-03-30T15:35:40.792Z",
    "last_allocation_status" : "no"
  },
  "can_allocate" : "no",
  "allocate_explanation" : "Elasticsearch isn't allowed to allocate this shard to any of the nodes in the cluster. Choose a node to which you expect this shard to be allocated, find this node in the node-by-node explanation, and address the reasons which prevent Elasticsearch from allocating this shard there.",
  "node_allocation_decisions" : [
    {
      "node_id" : "YjIWHJOBTUujt4Uo9SLQ8g",
      "node_name" : "elasticsearch",
      "transport_address" : "172.18.0.2:9300",
      "node_attributes" : {
        "ml.allocated_processors" : "4",
        "ml.machine_memory" : "8320856064",
        "transform.config_version" : "10.0.0",
        "xpack.installed" : "true",
        "ml.config_version" : "12.0.0",
        "ml.max_jvm_size" : "536870912",
        "ml.allocated_processors_double" : "4.0"
      },
      "roles" : [
        "data",
        "data_cold",
        "data_content",
        "data_frozen",
        "data_hot",
        "data_warm",
        "ingest",
        "master",
        "ml",
        "remote_cluster_client",
        "transform"
      ],
      "node_decision" : "no",
      "weight_ranking" : 1,
      "deciders" : [
        {
          "decider" : "disk_threshold",
          "decision" : "NO",
          "explanation" : "the node is above the high watermark cluster setting [cluster.routing.allocation.disk.watermark.high=90%], having less than the minimum required [2.1gb] free space, actual free: [361.4mb], actual used: [98.3%]"
        }
      ]
    }
  ]
} 
```                                                         
                                                         ```
```sh
curl -sv -XPOST http://localhost:8080/basic/publish -d '{"name": "value"}'
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /basic/publish HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-Length: 17
> Content-Type: application/x-www-form-urlencoded
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 
< Content-Length: 0
< Date: Mon, 30 Mar 2026 17:21:32 GMT
< 
* Connection #0 to host localhost left intact

```

```sh
docker-compose exec kafka sh
```
```sh
kafka-topics --bootstrap-server localhost:9092 --list
```

```text
demo-topic
```

```sh
TOPIC=demo-topic
kafka-console-consumer --bootstrap-server localhost:9092   --topic $TOPIC  --from-beginning

```

```text
%7B%22name%22%3A+%22value%22%7D=
```

but
```text
app2            java -javaagent:/home/elas ...   Restarting       
```

and 
```text

Error starting ApplicationContext. To display the conditions report re-run your application with 'debug' enabled.
2026-03-30 17:24:40.706 ERROR 1 --- [           main] o.s.boot.SpringApplication               : Application run failed

org.springframework.context.ApplicationContextException: Failed to start bean 'org.springframework.kafka.config.internalKafkaListenerEndpointRegistry'; nested exception is java.lang.IllegalStateException: No group.id found in consumer config, container properties, or @KafkaListener annotation; a group.id is required when group management is used.
	at org.springframework.context.support.DefaultLifecycleProcessor.doStart(DefaultLifecycleProcessor.java:182) ~[spring-context-5.3.31.jar!/:5.3.31]
	at org.springframework.context.support.DefaultLifecycleProcessor.access$200(DefaultLifecycleProcessor.java:54) ~[spring-context-5.3.31.jar!/:5.3.31]
...
	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:65) ~[app.jar:0.1.0-SNAPSHOT]
Caused by: java.lang.IllegalStateException: 
No group.id found in consumer config, container properties, or @KafkaListener annotation; 
a group.id is required when group management is used.
	at org.springframework.util.Assert.state(Assert.java:76) ~[spring-core-5.3.31.jar!/:5.3.31]
	at org.springframework.kafka.listener.AbstractMessageListenerContainer.checkGroupId(AbstractMessageListenerContainer.java:524) ~[spring-kafka-2.8.11.jar!/:2.8.11]
	at org.springframework.kafka.listener.AbstractMessageListenerContainer.start(AbstractMessageListenerContainer.java:456) ~[spring-kafka-2.8.11.jar!/:2.8.11]
	at org.springframework.kafka.config.KafkaListenerEndpointRegistry.startIfNecessary(KafkaListenerEndpointRegistry.java:363) ~[spring-kafka-2.8.11.jar!/:2.8.11]
	at org.springframework.kafka.config.KafkaListenerEndpointRegistry.start(KafkaListenerEndpointRegistry.java:308) ~[spring-kafka-2.8.11.jar!/:2.8.11]
	at org.springframework.context.support.DefaultLifecycleProcessor.doStart(DefaultLifecycleProcessor.java:179) ~[spring-context-5.3.31.jar!/:5.3.31]
	... 21 common frames omitted

```


after fixing the `spring.kafka.consumer.group-id` the log becomes
```txt
app2             | 2026-03-30 19:01:16.280  INFO 1 --- [ntainer#0-0-C-1] org.apache.kafka.clients.Metadata        : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Cluster ID: MkU3OEVBNTcwNTJENDM2Qg
app2             | 2026-03-30 19:01:16.287  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Discovered group coordinator kafka:9092 (id: 2147483646 rack: null)
app2             | 2026-03-30 19:01:16.295  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] (Re-)joining group
app2             | 2026-03-30 19:01:16.330  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Request joining group due to: need to re-join with the given member-id
app2             | 2026-03-30 19:01:16.332  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] (Re-)joining group
app2             | 2026-03-30 19:01:19.336  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Successfully joined group with generation Generation{generationId=1, memberId='consumer-demo-group-1-1239aa74-b522-4140-90cc-7f7e175eb552', protocol='range'}
app2             | 2026-03-30 19:01:19.345  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Finished assignment for group at generation 1: {consumer-demo-group-1-1239aa74-b522-4140-90cc-7f7e175eb552=Assignment(partitions=[demo-topic-0])}
app2             | 2026-03-30 19:01:19.360  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Successfully synced group in generation Generation{generationId=1, memberId='consumer-demo-group-1-1239aa74-b522-4140-90cc-7f7e175eb552', protocol='range'}
app2             | 2026-03-30 19:01:19.361  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Notifying assignor about the new Assignment(partitions=[demo-topic-0])
app2             | 2026-03-30 19:01:19.364  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Adding newly assigned partitions: demo-topic-0
app2             | 2026-03-30 19:01:19.371  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Found no committed offset for partition demo-topic-0
app2             | 2026-03-30 19:01:19.382  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Found no committed offset for partition demo-topic-0
app2             | 2026-03-30 19:01:19.412  INFO 1 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.SubscriptionState    : [Consumer clientId=consumer-demo-group-1, groupId=demo-group] Resetting offset for partition demo-topic-0 to position FetchPosition{offset=1, offsetEpoch=Optional.empty, currentLeader=LeaderAndEpoch{leader=Optional[kafka:9092 (id: 1 rack: null)], epoch=0}}.
app2             | 2026-03-30 19:01:19.454  INFO 1 --- [ntainer#0-0-C-1] o.s.k.l.KafkaMessageListenerContainer    : demo-group: partitions assigned: [demo-topic-0]
```

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
%7B%22name%22%3A+%22value%22%7D=
%7B%22name%22%3A+%22value%22%7D=
^CProcessed a total of 2 messages
```

```sh
docker-compose logs app2 
```
```text
app2             | received: %7B%22name%22%3A+%22value%22%7D=```

```

```sh
for i in 1 2 3 4 5; do
  curl -XPOST http://localhost:8080/basic/publish \
    -H "Content-Type: application/json" \
    -d "{\"id\":$i,\"name\":\"value$i\"}"
done
```

The Elastic APM is operational:

[Kibana starting](screenshots/capture-kibana-launching.png)


Both services are known to Elastic

[Service Visibility](screenshots/capture-kibana-observability-apm-services.png)

However the trace context propagation over Kafka is not yet happening

[Traces Visibility](screenshots/capture-kibana-observability-apm-traces.png)


Obsertvability → APM → Services
app1
app2

Observability → APM → Traces
