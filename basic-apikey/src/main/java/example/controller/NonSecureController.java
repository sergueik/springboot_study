package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class NonSecureController {

	private static final Logger logger = LoggerFactory
			.getLogger(NonSecureController.class);

	@SuppressWarnings("rawtypes")
	@GetMapping(value = "/api/v1/nonsecure")
	public ResponseEntity nonsecure() {
		logger.info("Processing request: /api/v1/nonsecure");
		return ResponseEntity.ok().build();
	}
}
