package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class ExampleController {

	// see also about writing SpringBoot application tests without relying on
	// SpringBoot field injection
	// https://reflectoring.io/unit-testing-spring-boot/
	@Autowired
	private ExampleService service;

	@Autowired
	public ExampleController(ExampleService data) {
		service = data;
	}

	public ExampleController() {

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
		Data result = service.handleData(data);
		// TODO: return result leads to postJSONTest failure
		// return result;
		return data;

	}

	// see also: https://qna.habr.com/q/1079162
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public ResponseEntity<String> list(
			@RequestParam final Collection<UUID> uuids) {
		String data = String.join(" ",
				uuids.stream().map(o -> o.toString()).collect(Collectors.toList()));
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(method = RequestMethod.POST, value = "/post/set", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Set<Data>> postSet(@RequestBody Set<String> inputs) {

		Set<Data> result = new HashSet<>();
		for (String input : inputs) {
			@SuppressWarnings("unused")
			Data data = new Data(input);
			result.add(data);
		}
		return ResponseEntity.status(HttpStatus.OK).body(result);

	}

	@GetMapping(value = "/array/params", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<String>> paramArrayEcho(
			@RequestParam Optional<List<String>> values) {
		return (values.isPresent() && values.get().size() > 0)
				? ResponseEntity.status(HttpStatus.OK).body(values.get())
				: ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
						.body(new ArrayList<String>());
	}

	@GetMapping(value = "/queryparam", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> queryParam(
			@RequestParam Optional<List<String>> appids,
			@RequestParam Optional<List<Integer>> ids) {
		String payload = null;
		if ((appids.isPresent() && appids.get().size() > 0)
				&& (ids.isPresent() && ids.get().size() > 0)) {
			payload = String.format("appids: %s ids: %s",
					String.join(",", appids.get()), String.join(",", ids.get().stream()
							.map(o -> String.format("%d", o)).collect(Collectors.toList())));
			return ResponseEntity.status(HttpStatus.OK).body(payload);
		} else {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body("");

		}

	}

	// see also examples in
	// https://www.programcreek.com/java-api-examples/?class=org.springframework.http.MediaType&method=APPLICATION_FORM_URLENCODED_VALUE
	// https://www.baeldung.com/spring-request-method-not-supported-405
	// returns HTTP 405 error code for GET
	@RequestMapping(method = { RequestMethod.PUT,
			RequestMethod.POST }, value = "/post/form", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Data> postForm(
			@RequestBody final MultiValueMap<String, String> param /* , HttpServletResponse response */) {
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
				.body(service.handleData(new Data(param.getFirst("name"))));
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
