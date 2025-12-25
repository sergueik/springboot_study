package com.bookportal.api.repository;

import com.bookportal.api.entity.User;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface UserRepository extends ReactiveCrudRepository<User, String> {
    Mono<User> findByIdAndActiveTrue(String id);
    Mono<User> findByMail(String mail);
    Mono<User> findByMailAndActiveTrue(String mail);
    Mono<User> findByMailAndActiveFalse(String mail);
}