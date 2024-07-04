package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
// https://www.baeldung.com/spring-response-entity
import org.springframework.http.ResponseEntity.BodyBuilder;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.SimpleService;

@RestController
@RequestMapping("/dispatch")
public class DispatchController {

	@Autowired
	private SimpleService simpleService;

	@Autowired
	public DispatchController(SimpleService data) {
		simpleService = data;
	}

	public DispatchController() {

	}

	@RequestMapping(method = RequestMethod.GET, value = "/call", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	// NOTE; NPE during test when testing public ResponseEntity<?> callService(
	public ResponseEntity<String> callService(
			@RequestParam(defaultValue = "test") String name) {
		try {
			String result = simpleService.hello(name);
			return ResponseEntity.status(HttpStatus.OK).body(result);
		} catch (NullPointerException e) {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).build();
			// NOTE: another useful status HttpStatus.PRECONDITION_FAILED.valur() 412
		} catch (Exception e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
	}

	@RequestMapping(method = RequestMethod.GET, value = "/call2", produces = {
			MediaType.APPLICATION_JSON_VALUE })

	public String callService(@RequestParam(defaultValue = "test") String name,
			HttpServletResponse response) {
		try {
			String result = simpleService.hello(name);
			response.setStatus(HttpStatus.OK.value());
			return result;
		} catch (NullPointerException e) {
			response.setStatus(HttpStatus.METHOD_NOT_ALLOWED.value());
			// response.setStatus(HttpStatus.PRECONDITION_FAILED.value());
			return null;
		} catch (Exception e) {
			response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
			return null;
		}
	}

}
