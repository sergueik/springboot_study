package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
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
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.service.ProcessExpressionService;

@RestController
@RequestMapping("/")
public class RegexpValidationController {

	private boolean debug = false;
	@Autowired
	private ProcessExpressionService service;

	public void setDebug(boolean data) {
		debug = data;
	}

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	@ResponseBody
	@PostMapping(value = "/validate", consumes = {
			MediaType.APPLICATION_FORM_URLENCODED_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validate(
			@RequestParam String expression, @RequestParam Optional<Boolean> debug) {

		// https://www.baeldung.com/spring-response-header
		// https://stackoverflow.com/questions/31612931/cors-issue-on-localhost-while-calling-rest-service-from-angularjs
		HttpHeaders responseHeaders = new HttpHeaders();
		responseHeaders.set("Access-Control-Allow-Origin", "*");

		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body(service.processExpression(expression));
	}

	@ResponseBody
	@RequestMapping(method = RequestMethod.OPTIONS, value = "/validate_cors")
	public ResponseEntity<String> validateCorsOptions() {
		HttpHeaders responseHeaders = new HttpHeaders();
		responseHeaders.set("Access-Control-Allow-Origin", "*");
		responseHeaders.set("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
		responseHeaders.set("Access-Control-Allow-Headers", "Content-Type");
		responseHeaders.setAccessControlAllowOrigin("*");
		responseHeaders.setAccessControlAllowHeaders(Arrays.asList("Content-Type"));
		responseHeaders.setAccessControlAllowMethods(
				Arrays.asList(HttpMethod.GET, HttpMethod.POST, HttpMethod.OPTIONS));
		log.info("Returning headers: " + responseHeaders);
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body("");
	}

	@ResponseBody
	@PostMapping(value = "/validate_cors", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validateCors(
			@RequestBody Map<String, Object> data) {

		// https://www.baeldung.com/spring-response-header
		// https://stackoverflow.com/questions/31612931/cors-issue-on-localhost-while-calling-rest-service-from-angularjs
		HttpHeaders responseHeaders = new HttpHeaders();
		responseHeaders.set("Access-Control-Allow-Origin", "*");
		responseHeaders.set("Access-Control-Allow-Origin", "*");
		responseHeaders.set("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
		responseHeaders.set("Access-Control-Allow-Headers", "Content-Type");
		responseHeaders.setAccessControlAllowOrigin("*");
		responseHeaders.setAccessControlAllowHeaders(Arrays.asList("Content-Type"));
		responseHeaders.setAccessControlAllowMethods(
				Arrays.asList(HttpMethod.GET, HttpMethod.POST, HttpMethod.OPTIONS));
		log.info("Returning headers: " + responseHeaders);
		// Map<String, Object> data = new HashMap<>();
		data.put("status", "OK");
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body(data);
	}

}
