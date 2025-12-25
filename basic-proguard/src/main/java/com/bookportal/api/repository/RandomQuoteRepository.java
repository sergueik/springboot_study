package com.bookportal.api.repository;

import com.bookportal.api.entity.RandomQuote;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface RandomQuoteRepository extends ReactiveCrudRepository<RandomQuote, String> {
    Mono<RandomQuote> findByIdAndActiveTrue(String id);
}
