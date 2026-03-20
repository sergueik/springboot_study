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

### See Also
  * the original [source fork](https://github.com/alephzed/baeldung-tutorials/blob/main/spring-kafka/pom.xml) - favors the testcontainers over embedded
  * https://docs.spring.io/spring-kafka/api/org/springframework/kafka/test/context/EmbeddedKafka.html
  * https://folio-org.atlassian.net/wiki/spaces/FOLIJET/pages/1386048/How+to+Use+Testcontainers+in+Integration+Tests
  * https://testcontainers.com/guides/
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
