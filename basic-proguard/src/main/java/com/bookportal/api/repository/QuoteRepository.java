package com.bookportal.api.repository;

import com.bookportal.api.entity.Quote;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface QuoteRepository extends ReactiveCrudRepository<Quote, String> {
    Mono<Quote> findQuoteByIdAndActiveTrue(String id);
}
