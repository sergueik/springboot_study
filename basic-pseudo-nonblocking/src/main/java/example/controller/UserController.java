package example.controller;

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

import java.util.Map;
import java.util.concurrent.Callable;
import java.lang.Runnable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;

import example.model.User;
import example.service.UserService;

@RestController
@RequestMapping("/users")
public class UserController {

	private final Map<Long, User> users = new ConcurrentHashMap<>();

	@GetMapping(value = "/{id}", produces = { MediaType.APPLICATION_JSON_VALUE })
	public DeferredResult<HttpEntity<User>> getUser(@PathVariable("id") long id) {
		DeferredResult<HttpEntity<User>> result = new DeferredResult<>();
		// short-circuit request parameter validation before scheduling background work
		// but wrap in method signature 
		if (id <= 0) {
			// reserve METHOD_NOT_ALLOWED for invalid verbs
			result.setResult(ResponseEntity.status(HttpStatus.BAD_REQUEST)
					.body(new User(id, String.format("Method not allowed with Invalid id: %d", id))));
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

		ForkJoinPool.commonPool().submit(producer);
		return result;
	}

	@PostMapping(value = "", consumes = { MediaType.APPLICATION_JSON_VALUE }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public Callable<HttpEntity<User>> addUser(@RequestBody User user) {
		Callable<HttpEntity<User>> producer = () -> {
			users.put(user.getId(), user);
			return ResponseEntity.ok(user);
		};
		return producer;
	}
}
