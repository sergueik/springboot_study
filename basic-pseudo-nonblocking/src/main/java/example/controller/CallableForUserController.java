package example.controller;


/**
 * Copyright 2025 Serguei Kouzmine
 */

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpEntity;
import org.springframework.http.MediaType;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.Callable;
import java.lang.Runnable;
import java.net.URI;
import java.util.concurrent.ConcurrentHashMap;
import static java.util.concurrent.ForkJoinPool.commonPool;

import javax.annotation.PostConstruct;

import example.model.User;
import example.service.UserService;

@RestController
@RequestMapping("/callable/users")
public class CallableForUserController {

	private final Map<Long, User> users = new ConcurrentHashMap<>();
	
	@PostConstruct
	public void init() {
		// Add a default user after bean initialization
		users.put(1L, new User(1L, "Alice", "alice@example.com"));
	}

	@GetMapping("/{id}")
	public Callable<ResponseEntity<User>> getUser(@PathVariable long id) {
		return () -> {
			if (id <= 0) {
				return ResponseEntity.badRequest().body(new User(id, "Invalid id", null));
			}
			User user = users.get(id);
			if (user == null) {
				return ResponseEntity.notFound().build();
			}
			return ResponseEntity.ok(user);
		};
	}

	@PostMapping("")
	public Callable<ResponseEntity<User>> addUser(@RequestBody User user) {
		return () -> {
			users.put(user.getId(), user);
			return ResponseEntity.status(HttpStatus.CREATED).body(user);
			// return ResponseEntity.created(URI.create("/callable/users/" + user.getId())).body(user);
		};
	}

}
