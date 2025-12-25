package com.bookportal.api.util.mapper;

import com.bookportal.api.entity.Publisher;
import com.bookportal.api.model.PublisherDTO;
import reactor.core.publisher.Mono;

public class PublisherMapper {
    public static Mono<Publisher> dtoToPublisher(PublisherDTO dto) {
        return Mono.fromSupplier(() -> {
        Publisher publisher = new Publisher();
        publisher.setName(dto.getName());
        return publisher;
        });
    }
}
