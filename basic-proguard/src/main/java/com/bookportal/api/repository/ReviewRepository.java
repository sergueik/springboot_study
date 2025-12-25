package com.bookportal.api.repository;

import com.bookportal.api.entity.Review;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ReviewRepository extends ReactiveCrudRepository<Review, String> {
}
