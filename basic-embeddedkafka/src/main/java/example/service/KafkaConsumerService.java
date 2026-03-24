package example.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
public class KafkaConsumerService {

	private static final Logger log = LoggerFactory.getLogger(KafkaConsumerService.class);

	@KafkaListener(topics = "your-topic", groupId = "test-group")
	public void consume(String message) {
		log.info("Received: {}", message);

		// Force failure to trigger DLQ
		if (message.contains("fail")) {
			throw new RuntimeException("Simulated processing error");
		}

		log.info("Processed successfully");
	}
}