package example;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/hello")
public class Resource {

	private Service service;

	public Resource(Service data) {
		service = data;
	}

	@GetMapping
	public String helloWorld() {
		return service.hello();
	}

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		return new Data("Hello basic");
	}

	@PostMapping(value = "/post", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data post(@RequestBody Data data) {
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
