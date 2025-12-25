package com.bookportal.api.repository;

import com.bookportal.api.entity.Top20;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface Top20Repository extends ReactiveCrudRepository<Top20, String> {
}
