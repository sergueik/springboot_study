package example.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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

import com.google.gson.Gson;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class Controller {

	// @Autowired
	private ExampleService service;

	public Controller(ExampleService data) {
		service = data;
	}

	@GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE })
	public String hello() {
		return service.hello();
	}

	@GetMapping(value = "/json", produces = { MediaType.APPLICATION_JSON_VALUE })
	public Data json() {
		return new Data(service.hello());
	}

	@PostMapping(value = "/post", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public Data post(@RequestBody Data data) {
		return data;
	}

	// example execution:
	// GET http://192.168.0.64:8085/basic/params?values=a&values=b&values=c
	// 200 [ "a" ]
	// http://192.168.0.64:8085/basic
	// 405 []
	@GetMapping(value = "/params", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<List<String>> paramArrayEcho(
			@RequestParam Optional<List<String>> values) {
		return (values.isPresent() && values.get().size() > 0)
				? ResponseEntity.status(HttpStatus.OK).body(values.get())
				: ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
						.body(new ArrayList<String>());
	}

	private static final RestTemplate restTemplate = new RestTemplate();

	// @Value("${server.port:8085}")
	// private int port;
	// the @Value annotation is not working
	private int port = 8085;
	// when run test, seeing:
	// org.springframework.web.client.ResourceAccessException: I/O error on POST
	// request
	// for "http://localhost:0/basic/post": connect:
	// Address is invalid on local machine, or port is not valid on remote
	// machine

	@ResponseBody
	// 406 Not Acceptable client error response
	// 415 Unsupported Media Type
	@PostMapping(value = "/page" /*
																* , consumes = { MediaType.TEXT_PLAIN_VALUE }
																*/, produces = { MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> page(@RequestParam String name) {
		final String url = String.format("http://localhost:%d/basic/post", port);
		// perform API call to localhost

		final HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		final Data input = new Data();
		input.setName(name);
		final Gson gson = new Gson();

		final String payload = gson.toJson(input);
		System.err
				.println(String.format("POSTING %s to %s", payload.toString(), url));
		final HttpEntity<String> request = new HttpEntity<String>(payload, headers);
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
