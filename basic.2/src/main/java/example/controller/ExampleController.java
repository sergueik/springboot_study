package example.controller;

import java.util.Map;

import org.springframework.http.MediaType;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class ExampleController {

	@Autowired
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
	
	@RequestMapping(method = RequestMethod.POST, value = "/post/form", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data postForm(

			@RequestBody final MultiValueMap<String, String> param) {
		Data data = new Data(param.get("name").get(0));
		@SuppressWarnings("unused")
		Data result = service.handleData(data);
		System.err.println("Data from service: name=" + data.getName());
		// not serialized ??
		// return result;
		return data;
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
