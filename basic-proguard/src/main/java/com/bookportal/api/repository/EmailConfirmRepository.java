package com.bookportal.api.repository;

import com.bookportal.api.entity.EmailConfirm;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface EmailConfirmRepository extends ReactiveCrudRepository<EmailConfirm, String> {
    Mono<EmailConfirm> findBySecretKeyAndActiveTrue(String secretKey);
    Mono<EmailConfirm> findByUser_IdAndActiveTrue(String id);
}
