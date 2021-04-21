package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class ExampleController {

	// NOTE: one can skip annotation in favor of straight import and still have
	// one's integration tests valid
	// @Autowired
	private ExampleService service;

	public ExampleController(ExampleService data) {
		service = data;
	}

	@GetMapping
	public String hello() {
		return service.hello();
	}

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		return new Data(service.hello());
	}

	@RequestMapping(method = RequestMethod.POST, value = "/post/json", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data postJson(@RequestBody Data data) {
		@SuppressWarnings("unused")
		Data dummy = service.handleData(data);
		return data;

	}

	// see also examples in
	// https://www.programcreek.com/java-api-examples/?class=org.springframework.http.MediaType&method=APPLICATION_FORM_URLENCODED_VALUE
	// https://www.baeldung.com/spring-request-method-not-supported-405
	// returns HTTP 405 error code for GET
	@RequestMapping(method = { RequestMethod.PUT,
			RequestMethod.POST }, value = "/post/form", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Data> postForm(
			@RequestBody final MultiValueMap<String, String> param /*, HttpServletResponse response */) {
		if (param.isEmpty()) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new Data());
			// Alternatively change the method signature to include
			// HttpServletResponse response
			// and then
			// response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			// see also:
			// https://stackoverflow.com/questions/16232833/how-to-respond-with-http-400-error-in-a-spring-mvc-responsebody-method-returnin
		}
		return ResponseEntity.status(HttpStatus.OK)
				.body(service.handleData(new Data()));
	}

	public static class Data {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(String data) {
			name = data;
		}

		public Data(String name) {
			this.name = name;
		}

		public Data() {
		}
	}
}
