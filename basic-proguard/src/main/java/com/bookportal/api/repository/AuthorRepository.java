package com.bookportal.api.repository;

import com.bookportal.api.entity.Author;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface AuthorRepository extends ReactiveCrudRepository<Author, String> {
    Mono<Author> findByIdAndActiveTrue(String id);
}
