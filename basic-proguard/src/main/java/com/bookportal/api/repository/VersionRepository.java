package com.bookportal.api.repository;

import com.bookportal.api.entity.Version;
import com.bookportal.api.model.enums.EnvironmentEnum;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface VersionRepository extends ReactiveCrudRepository<Version, String> {
    Mono<Version> findByEnvironment(EnvironmentEnum env);
}
