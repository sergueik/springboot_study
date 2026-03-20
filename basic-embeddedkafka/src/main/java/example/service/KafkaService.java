package example.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

@Component
public class KafkaService {

	private static final Logger log = LoggerFactory.getLogger(KafkaService.class);

	@KafkaListener(topics = "input-topic", groupId = "demo-group")
	public void listen(String message) {
		log.info("Received: {}", message);

		if (message.contains("bad")) {
			log.info("Simulated processing failure: {}", message);
			throw new RuntimeException("Simulated processing failure");
		}
	}
}
