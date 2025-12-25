package com.bookportal.api.repository;

import com.bookportal.api.entity.Publisher;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface PublisherRepository extends ReactiveCrudRepository<Publisher, String> {
    Mono<Publisher> findByIdAndActiveTrue(String id);
}
