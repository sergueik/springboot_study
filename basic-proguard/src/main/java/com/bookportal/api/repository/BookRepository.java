package com.bookportal.api.repository;

import com.bookportal.api.entity.Book;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface BookRepository extends ReactiveCrudRepository<Book, String> {
    Mono<Book> findByIdAndActiveTrue(String id);
    Mono<Book> findByIdAndActiveTrueAndIsPublishedTrue(String id);
}
