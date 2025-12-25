package com.bookportal.api.repository;

import com.bookportal.api.entity.Comment;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CommentRepository extends ReactiveCrudRepository<Comment, String> {
}
