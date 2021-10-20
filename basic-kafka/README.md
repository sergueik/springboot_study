### Info

this directory containes

a trimmed down replica of Kafka Alpine based [Docker configuration](https://github.com/blacktop/docker-kafka-alpine/blob/master/2.3/Dockerfile)
with kafka stack and zookeeper in one image with updated release variable it pulls from [mirror](https://dlcdn.apache.org/kafka/)
 and a standalone ultra basic SpringBoot Kafka application messaging with itself


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
2021-10-21 01:37:19.876  INFO 6649 --- [ntainer#0-0-C-1] example.engine.Producer                  : #### -> Consumed message -> TIMESTAMP: 1634773039862
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
2021-10-21 01:37:22.888  INFO 6649 --- [ntainer#0-0-C-1] example.engine.Producer                  : #### -> Consumed message -> TIMESTAMP: 1634773042861
2021-10-21
offset: 63
key: subject
partition: 0
topic: test-topic
```
### TODO

the Springboot Kafka configuration is incompete: cannot direct it to use remote hosted Kafka:
```text
2021-10-20 19:32:50.818  WARN 4360 --- [ead | example-1] org.apache.kafka.client
s.NetworkClient   : [Producer clientId=example-1] Connection to node 1001 (local
host/127.0.0.1:9092) could not be established. Broker may not be available.
2021-10-20 19:32:50.821  WARN 4360 --- [ntainer#0-0-C-1] org.apache.kafka.client
s.NetworkClient   : [Consumer clientId=example-0, groupId=example] Connection to
 node 1001 (localhost/127.0.0.1:9092) could not be established. Broker may not b
e available.

```
### See Also

 * basic Golang client [example](https://github.com/blacktop/docker-kafka-alpine/tree/master/test/src)
