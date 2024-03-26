package example.controller;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.service.RegexpValidationService;

@RestController
@RequestMapping("/")
public class RegexpValidationController {

	private boolean debug = false;
	@Autowired
	private RegexpValidationService service;

	public void setDebug(boolean data) {
		debug = data;
	}

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	// NOTE: adding a @RequestParam without providing one from RestTemplate end
	// leads to all tests failing with a
	// org.springframework.web.client.HttpClientErrorException: 400 null
	@ResponseBody
	@PostMapping(value = "/validate_form", consumes = {
			MediaType.APPLICATION_FORM_URLENCODED_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validateForm(
			@RequestParam String expression, @RequestParam String pattern,
			@RequestParam Optional<Boolean> debug) {

		HttpHeaders responseHeaders = new HttpHeaders();
		responseHeaders.set("Access-Control-Allow-Origin", "*");

		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body(service.processExpression(expression));
	}

	@ResponseBody
	@CrossOrigin
	@PostMapping(value = "/validate_json", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validateAJAX(
			@RequestBody Map<String, Object> data) {

		HttpHeaders responseHeaders = new HttpHeaders();
		// NOTE: observed Chrome complain that the "*" is added twice but should not
		// - not reproduced in curl check
		responseHeaders.setAccessControlAllowOrigin("*");
		// NOTE: no "set" API - probably will never work when patched this way
		// NOTE: this is warning by Chrome
		// logged after the connection is actually made:
		// Refused to connect to '....' because
		// it violates the following Content Security Policy directive:
		// "default-src 'self' 'unsafe-inline' data:".
		// Note that 'connect-src' was not explicitly set,
		// so 'default-src' is used as a fallback.
		responseHeaders.add("Content-Security-Policy",
				"default-src ‘self’;*.mydomain.org");
		// see also: https://www.baeldung.com/spring-security-csp
		// responseHeaders.set("Access-Control-Allow-Origin", "*");
		String expression = data.get("expression").toString();
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body(service.processExpression(expression));
	}

}
