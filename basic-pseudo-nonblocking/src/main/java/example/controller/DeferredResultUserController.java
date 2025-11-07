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
import java.util.concurrent.ConcurrentHashMap;
import static java.util.concurrent.ForkJoinPool.commonPool;

import javax.annotation.PostConstruct;

import example.model.User;
import example.service.UserService;

@RestController
@RequestMapping("/deferred/users")
public class DeferredResultUserController {

	private final Map<Long, User> users = new ConcurrentHashMap<>();

	@PostConstruct
	public void init() {
		// Add a default user after bean initialization
		users.put(1L, new User(1L, "Alice", "alice@example.com"));
	}

	@GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
	public DeferredResult<ResponseEntity<Collection<User>>> getAllUsers() {
		DeferredResult<ResponseEntity<Collection<User>>> result = new DeferredResult<>();

		Runnable producer = () -> {
			try {
				Collection<User> allUsers = users.values();
				result.setResult(ResponseEntity.ok(allUsers));
			} catch (Exception ex) {
				result.setResult(ResponseEntity.status(HttpStatus.METHOD_FAILURE).build());
			}
		};

		commonPool().submit(producer);
		return result;
	}

	@GetMapping(value = "/{id}", produces = { MediaType.APPLICATION_JSON_VALUE })
	public DeferredResult<HttpEntity<User>> getUser(@PathVariable("id") long id) {
		DeferredResult<HttpEntity<User>> result = new DeferredResult<>();
		// short-circuit request parameter validation before scheduling background work
		// but wrap in method signature
		if (id <= 0) {
			// reserve METHOD_NOT_ALLOWED for invalid verbs
			result.setResult(ResponseEntity.status(HttpStatus.BAD_REQUEST)
					.body(new User(id, String.format("Method not allowed with Invalid id: %d", id), null)));
			return result;
		}
		Runnable producer = () -> {
			try {

				User user = users.get(id);

				if (user == null) {
					result.setResult(ResponseEntity.status(HttpStatus.NOT_FOUND).build());
				} else {
					result.setResult(ResponseEntity.status(HttpStatus.OK).body(user));
				}

			} catch (Exception ex) {
				result.setResult(ResponseEntity.status(HttpStatus.METHOD_FAILURE)
						.body(new User(-1L, "Error invoking service: " + ex.getMessage(), null)));
			}
		};

		commonPool().submit(producer);
		return result;
	}

	@PostMapping(value = "", consumes = { MediaType.APPLICATION_JSON_VALUE }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public Callable<HttpEntity<User>> addUser(@RequestBody User user) {
		Callable<HttpEntity<User>> producer = () -> {

			// assign an ID if missing
			if (user.getId() == null || user.getId() == 0L) {
				long newId = users.size() + 1L;
				user.setId(newId);
			}
			users.put(user.getId(), user);

			return ResponseEntity.status(HttpStatus.CREATED).body(user);
		};
		return producer;
	}
}
