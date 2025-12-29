package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import example.model.User;

@RestController
@RequestMapping("/users")
public class Controller {

	private final Map<Long, User> users = new ConcurrentHashMap<>();
	private final Gson gson = new Gson();
	private String payload;

	@PostConstruct
	public void init() {
		// Add a default user after bean initialization
		users.put(1L, new User(1L, "Alice", "alice@example.com"));
	}

	@PostMapping("")
	public ResponseEntity<User> addUser(@RequestBody User user) {
		users.put(user.getId(), user);
		return ResponseEntity.status(HttpStatus.CREATED).body(user);
	};

	@PutMapping(value = "/{id}")
	public ResponseEntity<User> put(@RequestBody Map<String, String> param, @PathVariable("id") long id) {
		User user = users.get(id);
		payload = gson.toJson(user);
		System.err.println(String.format("put updating user  %s", payload));
		payload = gson.toJson(param);
		System.err.println("with param: " + payload);

		if (user == null) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		System.err.println(
				String.format("put set name: \"%s\"", (param.containsKey("name") ? param.get("name") : "null")));
		if (!param.containsKey("name")) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		user.setName(param.get("name"));

		payload = gson.toJson(user);
		System.err.println("put end: " + payload);
		return ResponseEntity.ok(user);
	}

	@GetMapping("/{id}")
	public ResponseEntity<User> getUser(@PathVariable("id") long id) {
		User user = users.get(id);
		return ResponseEntity.ok(user);
	}

}
