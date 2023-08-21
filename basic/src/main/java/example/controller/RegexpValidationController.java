package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */


import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
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
		return ResponseEntity.status(HttpStatus.OK)
				.body(service.processExpression(expression));
	}

}
