package example.controller;

import org.springframework.http.HttpHeaders;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.kafka.support.KafkaHeaders;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
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

	@RequestMapping(method = RequestMethod.POST, value = "/publish", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> publish(final @RequestParam String topic, final @RequestBody String payload) {
		String message = String.format("published %d to %s", payload.length(), topic);
		HttpHeaders headers = new HttpHeaders();
		kafkaTemplate.send(MessageBuilder.withPayload(payload).setHeader(KafkaHeaders.TOPIC, topic).build());
		System.err.println(message);
		return new ResponseEntity<String>(message, headers, HttpStatus.OK);

	}
}
