package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
public class ExampleController {
	private boolean debug = false;
	private final KafkaTemplate<String, String> kafkaTemplate;

	public ExampleController(KafkaTemplate<String, String> kafkaTemplate) {
		this.kafkaTemplate = kafkaTemplate;
	}

	@GetMapping("/health")
	public ResponseEntity<Void> health() {
		return ResponseEntity.ok().build();
	}

	@PostMapping("/publish")
	public ResponseEntity<Void> publish(@RequestBody String body) {
		kafkaTemplate.send("demo-topic", body);
		return ResponseEntity.ok().build();
	}
}
