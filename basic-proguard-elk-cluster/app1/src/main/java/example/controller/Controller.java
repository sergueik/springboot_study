package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

import example.model.User;

@RestController
@RequestMapping("/users")
public class Controller {

	private User user;
	private final Gson gson = new Gson();
	private String payload;
	private RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<User> response = null;
	private String server = null;
	private String url = null;

	@Value("${server}")
	private String serverhost;

	@PostConstruct
	public void init() {
		// NOTE: 
		// intended to set the serverhost string to 
		// the name of the environment variable
		// SERVER 
		// but somehow serverhost is set to the value of that variable 
		// app2
		System.err.println("init read environment " + serverhost);
		// app1             | init read environment app2
		server = System.getenv(serverhost);
		System.err.println("init server= " + server);
		// app1             | init server= null`
		server = "app2";
	}

	@GetMapping("/{id}")
	public ResponseEntity<User> getUser(@PathVariable("id") long id) {
		Map<String, Object> uriVariables = new HashMap<>();
		url = String.format("http://%s:8080/users/{id}", server);
		uriVariables.put("id", id);
		try {
			response = restTemplate.getForEntity(url, User.class, uriVariables);
			return response;
		} catch (HttpClientErrorException.NotFound e) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		} catch (RestClientException e) {
			return ResponseEntity.status(HttpStatus.BAD_GATEWAY).body(null);
		}
	}

	@PostMapping("")

	public ResponseEntity<User> addUser(@RequestBody User data) {
		url = String.format("http://%s:8080/users", server);
		user = restTemplate.postForObject(url, data, User.class);
		return ResponseEntity.status(HttpStatus.CREATED).body(user);
	};

	@PutMapping(value = "/{id}")
	public ResponseEntity<User> put(@RequestBody Map<String, String> param, @PathVariable("id") long id) {
		url = String.format("http://%s:8080/users/{id}", server);
		Map<String, Object> uriVariables = new HashMap<>();
		uriVariables.put("id", id);
		user = restTemplate.getForObject(url, User.class, uriVariables);

		if (user == null) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		payload = gson.toJson(user);
		System.err.println(String.format("put updating user  %s", payload));
		payload = gson.toJson(param);
		System.err.println("with param: " + payload);

		System.err.println(
				String.format("put set name: \"%s\"", (param.containsKey("name") ? param.get("name") : "null")));
		if (!param.containsKey("name")) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		HttpEntity<Map<String, String>> requestEntity = new HttpEntity<>(param);
		response = restTemplate.exchange(url, HttpMethod.PUT, requestEntity, User.class);
		return ResponseEntity.ok(response.getBody());
	}
}
