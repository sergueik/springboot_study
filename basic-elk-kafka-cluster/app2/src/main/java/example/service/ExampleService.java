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
	// sweeten with pure Spring EL sugar layered on top of
	// legal Java annotation metadata annotation interface
	// evaluated at runtime
	@KafkaListener(topics = "${app.kafka.topic:demo-topic}", groupId = "${app.kafka.group-id:demo-group}")
	public void consume(ConsumerRecord<String, String> consumerRecord) {
		String payload = consumerRecord.value();
		System.out.println("received: " + payload);
		for (Header header : consumerRecord.headers()) {
			System.out.println(header.key() + " = " + new String(header.value()));
		}
	}
}
