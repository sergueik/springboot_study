### Info

this directory containes a trimmed down replica of Kafka Alpine based [Docker configuration](https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile)
with kafka stack and zookeeper in one image with updated release variable it pulls from [mirror](https://dlcdn.apache.org/kafka/)
 and a standalone ultra basic SpringBoot Kafka application messaging with itself
from [example Kafka Springboot project](https://github.com/layonez/kafka-example/tree/master/src/main/java/com/layo/kafkaexample) (sans its Docker part)

 and a plain java Kafka client example from
__Getting Started with Apache Kafka__ [course](https://app.pluralsight.com/library/courses/2802039d-8e70-4d95-a366-0f91744db191/table-of-contents)

### Usage

```sh
IMAGE=kafka-standalone
docker build -t $IMAGE -f Dockerfile .
```

* run zookeeper
```sh
IMAGE=kafka-standalone
NAME=zookeeper
docker run --name $NAME -d -p 2181:2181 $IMAGE zookeeper-server-start.sh config/zookeeper.properties
```
* run single kafka broker (partition)
```sh
NAME=kafka-1
IMAGE=kafka-standalone
docker run -d -v /var/run/docker.sock:/var/run/docker.sock -e KAFKA_ADVERTISED_HOST_NAME=localhost --link zookeeper -p 9092:9092 --name $NAME $IMAGE
```
* create topic with 1 partition (more possible by assigning unique ports /  hose names in the above command)

```sh
docker run --rm --link zookeeper $IMAGE kafka-topics.sh --create --zookeeper zookeeper:2181 --replication-factor 1 --partitions 1 --topic test-topic
```
this will print
```text
Created topic test-topic.
```
* run plain client
```sh
cd plain
mvn package
```
then run in two terminals side-by-side
```sh
java -cp target/example.plain-kafka.jar:target/lib/* example.ProducerApp
```
and
```java
java -cp target/examplelain-kafka.jar:target/lib/* example.BatchProducerApp
```
and
```sh
java -cp target/example.plain-kafka.jar:target/lib/* example.ConsumerApp
```
and
```sh
java -cp target/examplelain-kafka.jar:target/lib/* example.ConsumerGroupApp
```

the console logs will illustrate the execution:

```txt
created kafka producer: org.apache.kafka.clients.producer.KafkaProducer
producing messages
Message # 0 sent at 2021/10/21 15:13:12:561 to partition 0 offset 100
Message # 1 sent at 2021/10/21 15:13:13:579 to partition 0 offset 101
Message # 2 sent at 2021/10/21 15:13:14:584 to partition 0 offset 102
Message # 3 sent at 2021/10/21 15:13:15:592 to partition 0 offset 103
Message # 4 sent at 2021/10/21 15:13:16:597 to partition 0 offset 104
Message # 5 sent at 2021/10/21 15:13:17:602 to partition 0 offset 105
Message # 6 sent at 2021/10/21 15:13:18:611 to partition 0 offset 106
Message # 7 sent at 2021/10/21 15:13:19:617 to partition 0 offset 107
Message # 8 sent at 2021/10/21 15:13:20:621 to partition 0 offset 108
Message # 9 sent at 2021/10/21 15:13:21:626 to partition 0 offset 109
Message # 10 sent at 2021/10/21 15:13:22:630 to partition 0 offset 110
```

```text
assigned to partitions:
Partition: 0 in Topic: test-topic
Topic: test-topic, Partition: 0, Offset: 100, Key: null, Value: Message # 0
Topic: test-topic, Partition: 0, Offset: 101, Key: null, Value: Message # 1
Topic: test-topic, Partition: 0, Offset: 102, Key: null, Value: Message # 2
Topic: test-topic, Partition: 0, Offset: 103, Key: null, Value: Message # 3
Topic: test-topic, Partition: 0, Offset: 104, Key: null, Value: Message # 4
Topic: test-topic, Partition: 0, Offset: 105, Key: null, Value: Message # 5
Topic: test-topic, Partition: 0, Offset: 106, Key: null, Value: Message # 6
Topic: test-topic, Partition: 0, Offset: 107, Key: null, Value: Message # 7
Topic: test-topic, Partition: 0, Offset: 108, Key: null, Value: Message # 8
Topic: test-topic, Partition: 0, Offset: 109, Key: null, Value: Message # 9
Topic: test-topic, Partition: 0, Offset: 110, Key: null, Value: Message # 10
```
and
```text
subscribed to the following topics:
test-topic
Topic: test-topic, Partition: 0, Offset: 111, Key: null, Value: Message # 0
Topic: test-topic, Partition: 0, Offset: 112, Key: null, Value: Message # 1
Topic: test-topic, Partition: 0, Offset: 113, Key: null, Value: Message # 2
Topic: test-topic, Partition: 0, Offset: 114, Key: null, Value: Message # 3
Topic: test-topic, Partition: 0, Offset: 115, Key: null, Value: Message # 4
Topic: test-topic, Partition: 0, Offset: 116, Key: null, Value: Message # 5
Topic: test-topic, Partition: 0, Offset: 117, Key: null, Value: Message # 6
Topic: test-topic, Partition: 0, Offset: 118, Key: null, Value: Message # 7
Topic: test-topic, Partition: 0, Offset: 119, Key: null, Value: Message # 8
Topic: test-topic, Partition: 0, Offset: 120, Key: null, Value: Message # 9
Topic: test-topic, Partition: 0, Offset: 121, Key: null, Value: Message # 10
```
(the offset will grow)

* run springboot client
```sh
cd client
mvn spring-boot:run
```

this will start producing the log:
```text
Started Application in 2.405 seconds (JVM running for 4.019)
2021-10-21 01:37:16.905  INFO 6649 --- [   scheduling-1] o.a.k.clients.producer.ProducerConfig    : ProducerConfig values:
        acks = 1
        batch.size = 16384
        bootstrap.servers = [localhost:9092]
        buffer.memory = 33554432
        client.dns.lookup = default
        client.id = example-1
        compression.type = none
        connections.max.idle.ms = 540000
        delivery.timeout.ms = 120000
        enable.idempotence = false
        interceptor.classes = []
        key.serializer = class org.apache.kafka.common.serialization.StringSerializer
        linger.ms = 0
        max.block.ms = 60000
        max.in.flight.requests.per.connection = 5
        max.request.size = 1048576
        metadata.max.age.ms = 300000
        metadata.max.idle.ms = 300000
        metric.reporters = []
        metrics.num.samples = 2
        metrics.recording.level = INFO
        metrics.sample.window.ms = 30000
        partitioner.class = class org.apache.kafka.clients.producer.internals.DefaultPartitioner
        receive.buffer.bytes = 32768
        reconnect.backoff.max.ms = 1000
        reconnect.backoff.ms = 50
        request.timeout.ms = 30000
        retries = 2
        retry.backoff.ms = 100
        sasl.client.callback.handler.class = null
        sasl.jaas.config = null
        sasl.kerberos.kinit.cmd = /usr/bin/kinit
        sasl.kerberos.min.time.before.relogin = 60000
        sasl.kerberos.service.name = null
        sasl.kerberos.ticket.renew.jitter = 0.05
        sasl.kerberos.ticket.renew.window.factor = 0.8
        sasl.login.callback.handler.class = null
        sasl.login.class = null
        sasl.login.refresh.buffer.seconds = 300
        sasl.login.refresh.min.period.seconds = 60
        sasl.login.refresh.window.factor = 0.8
        sasl.login.refresh.window.jitter = 0.05
        sasl.mechanism = GSSAPI
        security.protocol = PLAINTEXT
        security.providers = null
        send.buffer.bytes = 131072
        ssl.cipher.suites = null
        ssl.enabled.protocols = [TLSv1.2]
        ssl.endpoint.identification.algorithm = https
        ssl.key.password = null
        ssl.keymanager.algorithm = SunX509
        ssl.keystore.location = null
        ssl.keystore.password = null
        ssl.keystore.type = JKS
        ssl.protocol = TLSv1.2
        ssl.provider = null
        ssl.secure.random.implementation = null
        ssl.trustmanager.algorithm = PKIX
        ssl.truststore.location = null
        ssl.truststore.password = null
        ssl.truststore.type = JKS
        transaction.timeout.ms = 60000
        transactional.id = null
        value.serializer = class org.apache.kafka.common.serialization.StringSerializer

2021-10-21 01:37:16.969  INFO 6649 --- [   scheduling-1] o.a.kafka.common.utils.AppInfoParser     : Kafka version: 2.5.1
2021-10-21 01:37:16.969  INFO 6649 --- [   scheduling-1] o.a.kafka.common.utils.AppInfoParser     : Kafka commitId: 0efa8fb0f4c73d92
2021-10-21 01:37:16.970  INFO 6649 --- [   scheduling-1] o.a.kafka.common.utils.AppInfoParser     : Kafka startTimeMs: 1634773036969
2021-10-21 01:37:17.233  INFO 6649 --- [ntainer#0-0-C-1] org.apache.kafka.clients.Metadata        : [Consumer clientId=example-0, groupId=example] Cluster ID: je-h1mT0RsOZhikng4OzGg
2021-10-21 01:37:17.235  INFO 6649 --- [ead | example-1] org.apache.kafka.clients.Metadata        : [Producer clientId=example-1] Cluster ID: je-h1mT0RsOZhikng4OzGg
2021-10-21 01:37:17.237  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.AbstractCoordinator  : [Consumer clientId=example-0, groupId=example] Discovered group coordinator localhost:9092 (id: 2147482646 rack: null)
2021-10-21 01:37:17.244  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.AbstractCoordinator  : [Consumer clientId=example-0, groupId=example] (Re-)joining group
2021-10-21 01:37:17.283  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.AbstractCoordinator  : [Consumer clientId=example-0, groupId=example] Join group failed with org.apache.kafka.common.errors.MemberIdRequiredException: The group member needs to have a valid member id before actually entering a consumer group
2021-10-21 01:37:17.285  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.AbstractCoordinator  : [Consumer clientId=example-0, groupId=example] (Re-)joining group
2021-10-21 01:37:17.309  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=example-0, groupId=example] Finished assignment for group at generation 3: {example-0-187c5e43-6400-4e7c-ae48-9626e49652ba=Assignment(partitions=[test-topic-0])}
2021-10-21 01:37:17.316  INFO 6649 --- [   scheduling-1] example.tasks.SendMessageTask            : Produced:
topic: test-topic
offset: 61
partition: 0
value size: 10
2021-10-21 01:37:17.327  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.AbstractCoordinator  : [Consumer clientId=example-0, groupId=example] Successfully joined group with generation 3
2021-10-21 01:37:17.344  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=example-0, groupId=example] Adding newly assigned partitions: test-topic-0
2021-10-21 01:37:17.369  INFO 6649 --- [ntainer#0-0-C-1] o.a.k.c.c.internals.ConsumerCoordinator  : [Consumer clientId=example-0, groupId=example] Setting offset for partition test-topic-0 to the committed offset FetchPosition{offset=61, offsetEpoch=Optional.empty, currentLeader=LeaderAndEpoch{leader=Optional[localhost:9092 (id: 1001 rack: null)], epoch=0}}
2021-10-21 01:37:17.371  INFO 6649 --- [ntainer#0-0-C-1] o.s.k.l.KafkaMessageListenerContainer    : example: partitions assigned: [test-topic-0]
2021-10-21 01:37:17.423  INFO 6649 --- [ntainer#0-0-C-1] example.engine.Producer                  : #### -> Consumed message -> TIMESTAMP: 1634773037235
2021-10-21
offset: 61
key: subject
partition: 0
topic: test-topic
2021-10-21 01:37:19.869  INFO 6649 --- [   scheduling-1] example.tasks.SendMessageTask            : Produced:
topic: test-topic
offset: 62
partition: 0
value size: 10
2021-10-21 01:37:19.876  INFO 6649 --- [ntainer#0-0-C-1] example.engine.Producer                  :
#### -> Consumed message -> TIMESTAMP: 1634773039862
2021-10-21
offset: 62
key: subject
partition: 0
topic: test-topic
2021-10-21 01:37:22.868  INFO 6649 --- [   scheduling-1] example.tasks.SendMessageTask            : Produced:
topic: test-topic
offset: 63
partition: 0
value size: 10
2021-10-21 01:37:22.888  INFO 6649 --- [ntainer#0-0-C-1] example.engine.Producer                  :
#### -> Consumed message -> TIMESTAMP: 1634773042861
2021-10-21
offset: 63
key: subject
partition: 0
topic: test-topic
```
### TODO

the Springboot Kafka configuration is incompete: cannot direct it to use remote hosted Kafka:
```text
2021-10-20 19:32:50.818  WARN 4360 --- [ead | example-1] org.apache.kafka.clients.NetworkClient   : [Producer clientId=example-1] Connection to node 1001 (localhost/127.0.0.1:9092) could not be established. Broker may not be available.
2021-10-20 19:32:50.821  WARN 4360 --- [ntainer#0-0-C-1] org.apache.kafka.clients.NetworkClient   : [Consumer clientId=example-0, groupId=example] Connection to node 1001 (localhost/127.0.0.1:9092) could not be established. Broker may not be available.
```
### See Also

  * Kafka quickstart [documentation](https://kafka.apache.org/quickstart)
  * basic Golang client [example](https://github.com/blacktop/docker-kafka-alpine/tree/master/test/src)
  * long [post](https://rmoff.net/2018/08/02/kafka-listeners-explained/) of challenges in Kafka network configuration, in particular explaining that Docker hosted instance will not be available to clients external to Docker host machine wants to connect
  * [stackoverflow thread](https://stackoverflow.com/questions/28146409/kafka-unable-to-send-a-message-to-a-remote-server-using-java) about the same problem
  * [discussion](https://stackoverflow.com/questions/23436613/how-can-i-convert-a-docker-image-into-a-vagrant-virtualbox-box) concertning wrong answers suggesting export binary image data to converting Dockerfile to Vagrantfile scenarios


### .Net Client


* Use prebuilt Vagrant box with Kafka and zookeeper hosted on the same vm, e.g. from [bitnami](https://docs.bitnami.com/virtual-machine/infrastructure/kafka/administration/run-producer-consumer/)
* Look for third party pure c# Client libraries e.g. [Jroland/kafka-net](https://github.com/Jroland/kafka-net) or [ntent/kafka4net](https://github.com/ntent/kafka4net) or [criteo/kafka-sharp](https://github.com/criteo/kafka-sharp) - many implementations are simply C# wrapper around the librdkafka native library. There may be version compatibility issues because underlying protocol changes

### See Also

  * Misc Bitnami configuration documents:

     +  https://docs.bitnami.com/virtual-machine/faq/get-started/find-credentials/
     +  https://docs.bitnami.com/virtual-machine/faq/get-started/connect-ssh/
     +  https://docs.bitnami.com/virtual-machine/faq/get-started/enable-ssh/
     +  https://docs.bitnami.com/virtual-machine/infrastructure/kafka/
     +  https://docs.bitnami.com/virtual-machine/infrastructure/kafka/administration/control-services/
     +  https://docs.bitnami.com/virtual-machine/infrastructure/kafka/administration/connect-remotely/
     +  https://docs.bitnami.com/virtual-machine/faq/administration/use-firewall/
     +  https://docs.bitnami.com/virtual-machine/infrastructure/kafka/administration/run-producer-consumer/
     +  https://docs.bitnami.com/virtual-machine/faq/get-started/enable-ssh-password/
     +  https://docs.bitnami.com/virtual-machine/infrastructure/kafka/administration/run-producer-consumer/

  * Distributed Tracing applications that subscribe to and publish data to Apache Kafka distributed event source and message queue

    + [documentation](https://newrelic.com/blog/how-to-relic/distributed-tracing-with-kafka) on how to achieve Tracing Kafka workflow with OpenTelemetry and newrelic
    + [discussion](https://community.dynatrace.com/t5/Alerting/Dynatrace-distributed-tracing-with-Kafka/td-p/126406)  of avalable options of handling Kafka workflows in Dynatrace and [some related Dynatrace documentation](https://www.dynatrace.com/support/help/how-to-use-dynatrace/services/service-detection-and-naming/service-types/define-messaging-services) - suport seems to be barely avialbale
    + [discussion](https://www.confluent.io/blog/importance-of-distributed-tracing-for-apache-kafka-based-applications/) on Importance of Distributed Tracing for Apache-Kafka-Based Applications and conflit with Kafka's primary ability to decouple producers and consumers using an event log as an intermediate layer
    + [overview of apm monitoring of kafka itself](https://dattell.com/data-architecture-blog/kafka-monitoring-with-elasticsearch-and-kibana/), not distributed tracing
    + [how to monitor a standalone Kafka cluster with Metricbeat and Filebeat or with Elastic Agent](https://www.elastic.co/blog/how-to-monitor-containerized-kafka-with-elastic-observability)
    + [discussion](https://discuss.elastic.co/t/apm-distributed-tracing-with-kafka/268759) of can one do distributed tracing with kafka in the middle, solving challenge of discovering the consumer (no affirmative conclusion)
    + [part1](https://www.elastic.co/blog/just-enough-kafka-for-the-elastic-stack-part1) and [part2](https://www.elastic.co/blog/just-enough-kafka-for-the-elastic-stack-part2) on using Kafka with Elastic Search, but as utility, not as a monitored target
    
    
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
