package com.bookportal.api.repository;

import com.bookportal.api.entity.UserBook;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserBookRepository extends ReactiveCrudRepository<UserBook, String> {
}
