package example.controller;

import org.springframework.http.MediaType;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/basic")
public class Controller {

	private static final Logger logger = LoggerFactory.getLogger(Controller.class);

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.ok().build();
	}

}
