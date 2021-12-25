package example.controller;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class Controller {

	// @Autowired
	private ExampleService service;

	public Controller(ExampleService data) {
		service = data;
	}

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public String hello() {
		return service.hello();
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
