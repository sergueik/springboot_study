package example.controller;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
public class StatusCodeController {

	private String payload = null;

	@GetMapping(value = "statuscode", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<String> statusCode(@RequestParam int code) {
		//
		HttpStatus status = HttpStatus.valueOf(code);
		// NOTE: enums ?
		// StatusCode statusCode = StatusCode(code);
		ResponseEntity<String> responseEntity = new ResponseEntity<String>(payload, status);
		return responseEntity;
	}

}
