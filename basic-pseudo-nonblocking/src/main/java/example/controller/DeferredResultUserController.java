package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.beans.factory.annotation.Autowired;
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
import java.util.List;

import java.util.Optional;
import java.util.concurrent.Callable;
import java.lang.Runnable;
import static java.util.concurrent.ForkJoinPool.commonPool;

import javax.annotation.PostConstruct;

import example.model.User;
import example.service.UserService;

@RestController
@RequestMapping("/deferred/users")
public class DeferredResultUserController {

	private final UserService userService;

	@Autowired
	public DeferredResultUserController(UserService userService) {
		this.userService = userService;
	}

	@PostConstruct
	public void init() {
		// Add a default user after bean initialization
		userService.createUser(new User(1L, "Alice", "alice@example.com"));
	}

	@SuppressWarnings("deprecation")
	@GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
	public DeferredResult<ResponseEntity<Collection<User>>> getAllUsers() {
		DeferredResult<ResponseEntity<Collection<User>>> result = new DeferredResult<>();

		Runnable producer = () -> {
			try {
				// Collection<User> allUsers = users.values();
				List<User> allUsers = userService.getAllUsers();
				result.setResult(ResponseEntity.ok(allUsers));
			} catch (Exception ex) {
				result.setResult(ResponseEntity.status(HttpStatus.METHOD_FAILURE).build());
			}
		};

		commonPool().submit(producer);
		return result;
	}

	@SuppressWarnings("deprecation")
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

				Optional<User> user = userService.getUser(id);
				if (user.isEmpty()) {
					result.setResult(ResponseEntity.status(HttpStatus.NOT_FOUND).build());
				} else {
					result.setResult(ResponseEntity.status(HttpStatus.OK).body(user.get()));
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

			userService.createUser(user);
			return ResponseEntity.status(HttpStatus.CREATED).body(user);
		};
		return producer;
	}
}
