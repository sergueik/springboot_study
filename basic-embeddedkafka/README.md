### Info

Baeldung Spring Boot Kafka testing approach without Testcontainers and without Docker, by using embedded Kafka instead


this is a well-supported path and is commonly used for early stage integration tests of 
publish messages to a topic → ✔ supported

Consume messages → ✔ supported

Simulate “bad message → DLQ” → ✔ can be modeled

No Docker / host constraints, emulating enterprise restrictions → ✔ using embedded broker


### See Also
  * the original [source fork](https://github.com/alephzed/baeldung-tutorials/blob/main/spring-kafka/pom.xml) - favors the testcontainers over embedded
