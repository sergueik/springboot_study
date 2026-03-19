package example.controller;

import org.springframework.beans.factory.annotation.Value;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Base64;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/")
public class Controller {
	private static final Logger logger = LoggerFactory.getLogger(Controller.class);

	private static final StringBuilder stringBuilder = new StringBuilder();
	@Value("${value}")
	private int value = 42;
	private final static String name = "question";

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.ok().build();
	}

	@GetMapping("cookie")
	public ResponseEntity<String> setCookie() {
		stringBuilder.append("value=" + value);
		byte[] bytes = stringBuilder.toString().getBytes();
		String encodedString = Base64.getEncoder().encodeToString(bytes);
		logger.info("data: {}/{}", stringBuilder.toString(), encodedString);

		ResponseCookie responseCookie = ResponseCookie.from(name, encodedString).httpOnly(true).secure(true).path("/")
				.maxAge(1 * 24 * 60 * 60).domain("localhost").build();
		return ResponseEntity.ok().header(HttpHeaders.SET_COOKIE, responseCookie.toString()).build();

	}

}
