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

	@ResponseBody
	@PostMapping(value = "/validate_form", consumes = {
			MediaType.APPLICATION_FORM_URLENCODED_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> validateForm(
			@RequestParam String expression, @RequestParam Optional<Boolean> debug) {

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
		responseHeaders.setAccessControlAllowOrigin("*");
		// responseHeaders.set("Access-Control-Allow-Origin", "*");
		String expression = data.get("expression").toString();
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders)
				.body(service.processExpression(expression));
	}

}

