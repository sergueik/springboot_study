package com.bookportal.api.repository;

import com.bookportal.api.entity.Vote;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface VoteRepository extends ReactiveCrudRepository<Vote, String> {
    Mono<Vote> findByUserIdAndBookId(String userId, String bookId);
}
