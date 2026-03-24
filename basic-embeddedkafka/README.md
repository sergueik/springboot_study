### Info

[Baeldung](https://www.baeldung.com/spring-boot-kafka-testing)  __Spring Boot Kafka__ testing approach without [Testcontainers](https://docs.spring.io/spring-boot/reference/testing/testcontainers.html) and without [Docker](), by using [embedded Kafka](https://www.limepoint.com/blog/exploring-embeddedkafka-and-kafkacontainers-in-spring-boot) instead



this is a well-supported path and is commonly used for early stage integration tests of 
publish messages to a topic → ✔ supported

Consume messages → ✔ supported

Simulate “bad message → DLQ” → ✔ can be modeled

No Docker / host constraints, emulating enterprise restrictions → ✔ using embedded broker

### TODO

Explain the
Declaring a bean ≠ it is used issue in [1a104b43b4](https://github.com/sergueik/springboot_study/commit/1a104b43b4416061b9b2c8cab42b8df4a9bf36f7)


### Switching to Real Kafka

Run Kafka in Docker in KRaft mode.

Update `docker-compose.yaml`:

```yaml
    environment:
      CLUSTER_ID: MkU3OEVBNTcwNTJENDM2Qk
      KAFKA_ADVERTISED_LISTENERS: PLAINTEXT://192.168.99.100:9092
```
use any valid base64 string for `CLUSTER_ID` and the external IP address of the docker machine host

Run `docker-compose`. Confirm through logs it is done starting -  troubleshoot  errors when there are
```sh
docker-compose logs kafka
```
```text
kafka | [2026-03-24 00:47:59,466] INFO [KafkaRaftServer nodeId=1] Kafka Server started (kafka.server.KafkaRaftServer)
```
configure the Java app to  use the docker machine host
```java
spring.kafka.bootstrap-servers=192.168.99.100:9092
```
Run the Java app
```sh
mvn spring-boot:run
```
Connect to kafka container and confirm the topics are present
```sh
docker exec -it kafka bash
```
```sh
kafka-topics --bootstrap-server localhost:9092 --list
```

```text
__consumer_offsets
input-topic
your-topic
```
Post some messagees
```sh
curl -XPOST http://localhost:8080/kafka/send -H "Content-Type: application/x-www-form-urlencoded" -d "msg=hello kafka"
```
```text
sent
```
Examine the topic in the Docker container
```sh
kafka-console-consumer --bootstrap-server localhost:9092   --topic your-topic   --from-beginning
```
```text
hello kafka
```
### See Also
  * the original [source fork](https://github.com/alephzed/baeldung-tutorials/blob/main/spring-kafka/pom.xml) - favors the testcontainers over embedded
  * https://docs.spring.io/spring-kafka/api/org/springframework/kafka/test/context/EmbeddedKafka.html
  * https://folio-org.atlassian.net/wiki/spaces/FOLIJET/pages/1386048/How+to+Use+Testcontainers+in+Integration+Tests
  * https://testcontainers.com/guides/
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
