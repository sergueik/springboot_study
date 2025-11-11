package example.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.concurrent.Callable;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import example.model.User;

@RestController
@RequestMapping("/users")
public class UserController {

	@PostMapping("/blocking")
	public ResponseEntity<String> createBlocking(@Valid @RequestBody User user) {
		return ResponseEntity.ok("Created user: " + user.getName());
	}

	@PostMapping("/callable")
	public Callable<ResponseEntity<String>> createCallable(@Valid @RequestBody User user) {
		return () -> ResponseEntity.ok("Created user: " + user.getName());
	}

	@PostMapping("/deferred")
	public DeferredResult<ResponseEntity<String>> createDeferred(@Valid @RequestBody User user) {
		DeferredResult<ResponseEntity<String>> result = new DeferredResult<>();
		// Simulate async completion
		new Thread(() -> result.setResult(ResponseEntity.ok("Created user: " + user.getName()))).start();
		return result;
	}
}
