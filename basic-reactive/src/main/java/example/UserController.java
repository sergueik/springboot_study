package example;

import example.model.User;
import example.service.UserService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
public class UserController {

	private final UserService userService;

	@Autowired
	public UserController(UserService userService) {
		this.userService = userService;
	}

	@GetMapping("/api/users")
	public Flux<User> findAll() {
		return userService.findAll();
	}

	@GetMapping("/api/users/{id}")
	public Mono<User> findById(@PathVariable("id") String id) {
		return userService.findById(id);
	}

	@DeleteMapping("/api/users/{id}")
	public Mono<Void> delete(@PathVariable("id") String id) {
		return userService.delete(id);
	}

	@PatchMapping("/api/users/{id}")
	public Mono<User> update(@PathVariable("id") String id, @RequestBody User user) {
		return userService.update(id, user);
	}

	@PostMapping("/api/users")
	public Mono<User> save(@RequestBody User user) {
		return userService.save(user);
	}
}
