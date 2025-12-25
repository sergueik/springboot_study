package com.bookportal.api.repository;

import com.bookportal.api.entity.HomePage;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface HomePageRepository extends ReactiveCrudRepository<HomePage, String> {
    Mono<HomePage> findByTypeAndBook_Id(String type, String id);
}
