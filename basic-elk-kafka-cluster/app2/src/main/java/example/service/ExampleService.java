package example.service;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Service
public class ExampleService {
	@KafkaListener(topics = "demo-topic", groupId = "demo-group")
	// TODO: configure topics and groupId

	public void consume(@Payload String payload) {
		System.out.println("received: " + payload);
	}
}
