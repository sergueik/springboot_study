package com.bookportal.api.repository;

import com.bookportal.api.entity.Favourite;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

@Repository
public interface FavouriteRepository extends ReactiveCrudRepository<Favourite, String> {
    Flux<Favourite> findAllByQuoteIdAndActiveTrue(String quoteId);
}
