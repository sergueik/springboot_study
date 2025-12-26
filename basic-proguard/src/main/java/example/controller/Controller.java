package example.controller;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import example.model.User;

@RestController
@RequestMapping("/users")
public class Controller {

	private final Map<Long, User> users = new ConcurrentHashMap<>();
	private Map<String, String> data = new HashMap<>();
	private final Gson gson = new Gson();
	private String payload;

	@PostConstruct
	public void init() {
		// Add a default user after bean initialization
		users.put(1L, new User(1L, "Alice", "alice@example.com"));
	}

	@GetMapping("/{id}")
	public ResponseEntity<User> getUser(@PathVariable long id) {
		User user = users.get(id);
		return ResponseEntity.ok(user);
	}

	@PostMapping("")
	public ResponseEntity<User> addUser(@RequestBody User user) {
		users.put(user.getId(), user);
		return ResponseEntity.status(HttpStatus.CREATED).body(user);
	};

	@PutMapping(value = "/{id}")
	public ResponseEntity<User> put(@RequestParam Optional<Map<String, String>> param, @PathVariable int id) {
		System.err.println("put begin");
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
				.body(new User());
		/*
		try {
			User user = users.get(id);
			payload = gson.toJson(user);
			System.err.println("put begin: " + payload);

			if (user == null) {
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
			if (!param.isPresent() || !param.get().containsKey("name")) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			user.setName(param.get().get("name"));

			payload = gson.toJson(user);
			System.err.println("put end: " + payload);
			return ResponseEntity.ok(user);
		} catch (Exception e) {
			System.err.println(String.format("Exception: %s", e.toString()));
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
					.body(new User());

		}
		*/
	}
}
