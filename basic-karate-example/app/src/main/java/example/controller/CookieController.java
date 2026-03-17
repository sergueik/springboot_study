package example.controller;

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

import java.util.ArrayList;
import java.util.Base64;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/")
public class CookieController {
	private static final Logger logger = LoggerFactory.getLogger(CookieController.class);

	private static final StringBuilder data = new StringBuilder();
	private final static int value = 42;
	private final static String name = "question";

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.ok().build();
	}

	@GetMapping("cookie")
	public ResponseEntity<String> setCookie() {
		data.append("value=" + value);
		byte[] bytes = data.toString().getBytes();
		String encoded = Base64.getEncoder().encodeToString(bytes);
		logger.info("data: {}/{}", data.toString(), encoded);

		ResponseCookie resCookie = ResponseCookie.from(name, encoded).httpOnly(true).secure(true).path("/")
				.maxAge(1 * 24 * 60 * 60).domain("localhost").build();
		return ResponseEntity.ok().header(HttpHeaders.SET_COOKIE, resCookie.toString()).build();

	}

}
