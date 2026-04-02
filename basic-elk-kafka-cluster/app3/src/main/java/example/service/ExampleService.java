package example.service;

/**

 * Copyright 2026 Serguei Kouzmine

 */

import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.messaging.handler.annotation.Payload;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.Header;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Service
public class ExampleService {
	@KafkaListener(topics = "${app.kafka.topic:demo-topic}", groupId = "${app.kafka.group-id:demo-group}")
	public void consume(@Payload String payload) {
		System.out.println("received: " + payload);
	}
}
