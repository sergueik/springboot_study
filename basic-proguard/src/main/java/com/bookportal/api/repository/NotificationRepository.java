package com.bookportal.api.repository;

import com.bookportal.api.entity.Notification;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NotificationRepository extends ReactiveCrudRepository<Notification, String> {
}
