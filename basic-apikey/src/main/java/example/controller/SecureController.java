package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SecureController {
	private static final Logger logger = LoggerFactory
			.getLogger(SecureController.class);

	@GetMapping(value = "/api/v1/secure")
	public String getName() {
		logger.info("Processing request: /api/v1/secure");
		return "Processed request: /api/v1/secure";
	}

}
