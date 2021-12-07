package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import example.service.ExampleService;
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

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		return new Data(service.hello());
	}

	@PostMapping(value = "/post", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data post(@RequestBody Data data) {
		return data;
	}

	private static final RestTemplate restTemplate = new RestTemplate();

	@ResponseBody
	@PostMapping(value = "/page", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> page(@RequestParam String name) {
		final String url = "http://localhost:/post";
		// perform API call to localhost

		final HttpHeaders headers = new HttpHeaders();

		final Data input = new Data();
		input.setName(name);
		final HttpEntity<String> request = new HttpEntity<String>(input.toString(),
				headers);
		final ResponseEntity<Data> responseEntity = restTemplate.postForEntity(url,
				request, Data.class, headers);
		final String result = responseEntity.getBody().getName();
		return ResponseEntity.status(HttpStatus.OK).body(result);
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
