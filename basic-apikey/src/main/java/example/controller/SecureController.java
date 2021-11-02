package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class SecureController {
	private static final Logger logger = LoggerFactory
			.getLogger(SecureController.class);

	@GetMapping(value = "secure")
	public String method1() {
		logger.info("Processing GET request: /api/v1/secure");
		return "Processed GET request: /api/v1/secure";
	}

	@PostMapping(value = "secure")
	public String method2() {
		logger.info("Processing POST request: /api/v1/secure");
		return "Processed POST request: /api/v1/secure";
	}

}
