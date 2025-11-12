package example.controller;

import example.model.User;
import example.repository.UserRepository;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/users/reactive")
public class UserReactiveController {

    private final UserRepository repository;

    public UserReactiveController(UserRepository repository) {
        this.repository = repository;
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public Mono<User> createUser(@Valid @RequestBody User user) {
        return repository.save(user);
    }

    @GetMapping("/{email}")
    public Mono<User> getUser(@PathVariable String email) {
        return repository.findByEmail(email);
    }

    @GetMapping
    public Flux<User> listUsers() {
        return repository.findAll();
    }

    @DeleteMapping("/{email}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public Mono<Void> deleteUser(@PathVariable String email) {
        return repository.deleteByEmail(email);
    }
}

