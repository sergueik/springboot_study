package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class NonSecureController {

	private static final Logger logger = LoggerFactory
			.getLogger(NonSecureController.class);

	@GetMapping(value = "nonsecure")
	public ResponseEntity<String> method1() {
		logger.info("Processing GET request: /api/v1/nonsecure");
		return ResponseEntity.ok().build();
	}

	@PostMapping(value = "nonsecure")
	public ResponseEntity<String> method2() {
		logger.info("Processing POST request: /api/v1/nonsecure");
		return ResponseEntity.ok().build();
	}
}
