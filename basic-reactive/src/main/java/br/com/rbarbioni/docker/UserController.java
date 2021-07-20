package br.com.rbarbioni.docker;

import br.com.rbarbioni.docker.model.User;
import br.com.rbarbioni.docker.service.UserService;
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
    public Flux<User> findAll(){
        return this.userService.findAll();
    }

    @GetMapping("/api/users/{id}")
    public Mono<User> findById(@PathVariable("id") String id){
        return this.userService.findById(id);
    }

    @DeleteMapping("/api/users/{id}")
    public Mono<Void> delete(@PathVariable("id") String id){
        return this.userService.delete(id);
    }

    @PatchMapping("/api/users/{id}")
    public Mono<User> update(@PathVariable("id") String id, @RequestBody User user){
        return this.userService.update(id, user);
    }

    @PostMapping("/api/users")
    public Mono<User> save(@RequestBody User user){
        return this.userService.save(user);
    }
}