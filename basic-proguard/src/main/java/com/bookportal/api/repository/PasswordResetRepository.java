package com.bookportal.api.repository;

import com.bookportal.api.entity.PasswordReset;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface PasswordResetRepository extends ReactiveCrudRepository<PasswordReset, String> {
    Mono<PasswordReset> findBySecretKeyAndActiveTrue(String secretKey);
}
