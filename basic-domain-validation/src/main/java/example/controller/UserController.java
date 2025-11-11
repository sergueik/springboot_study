package example.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.concurrent.Callable;


import javax.validation.Valid;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import example.model.User;

@RestController
@RequestMapping("/users")
public class UserController {

	// Blocking
	@PostMapping("/blocking")
	public ResponseEntity<String> createBlocking(@Valid @RequestBody User user) {
		return ResponseEntity.ok("Created user: " + user.getName());
	}

	// Callable async
	@PostMapping("/callable")
	public Callable<ResponseEntity<String>> createCallable(@Valid @RequestBody User user) {
		return () -> ResponseEntity.ok("Created user: " + user.getName());
	}

	// DeferredResult async
	@PostMapping("/deferred")
	public DeferredResult<ResponseEntity<String>> createDeferred(@Valid @RequestBody User user) {
		DeferredResult<ResponseEntity<String>> result = new DeferredResult<>();
		// Simulate async completion
		new Thread(() -> result.setResult(ResponseEntity.ok("Created user: " + user.getName()))).start();
		return result;
	}
}
