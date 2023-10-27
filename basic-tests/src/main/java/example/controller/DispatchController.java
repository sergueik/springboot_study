package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.SimpleService;

@RestController
@RequestMapping("/dispatch")
public class DispatchController {
	// NOTE: somewhat annoying, not reproduced in a clone project with no other
	// components but DispatchController SimpleService DispatchControllerTest
	// Exception encountered during context initialization - 
	// cancelling refresh attempt: 
	// org.springframework.beans.factory.UnsatisfiedDependencyException:
	// Error creating bean with name 'dispatchController' defined in file ...
	// Unsatisfied dependency expressed through constructor parameter 0;
	// nested exception is
	// org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'example.service.SimpleService' available:
	// expected at least 1 bean which qualifies as autowire candidate.
	// Dependency annotations: {}
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
		} catch (Exception e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
	}

}
