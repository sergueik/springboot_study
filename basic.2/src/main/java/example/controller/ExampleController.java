package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;
import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class ExampleController {

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

	@RequestMapping(method = RequestMethod.POST, value = "/post", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data post(@RequestBody Data data) {
		Data dummy = service.handleData(data);
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
