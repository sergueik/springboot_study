package example.repository;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import org.springframework.stereotype.Repository;

import java.util.concurrent.ConcurrentHashMap;

import example.model.User;

@Repository
public class UserRepository {

    private final ConcurrentHashMap<String, User> db = new ConcurrentHashMap<>();

    public Mono<User> save(User user) {
        db.put(user.getEmail(), user);
        return Mono.just(user);
    }

    public Mono<User> findByEmail(String email) {
        return Mono.justOrEmpty(db.get(email));
    }

    public Flux<User> findAll() {
        return Flux.fromIterable(db.values());
    }

    public Mono<Void> deleteByEmail(String email) {
        db.remove(email);
        return Mono.empty();
    }
}

