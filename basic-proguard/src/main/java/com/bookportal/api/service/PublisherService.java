package com.bookportal.api.service;

import com.bookportal.api.entity.Publisher;
import com.bookportal.api.exception.CustomAlreadyExistException;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.PublisherUpdateDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.PublisherRepository;
import com.bookportal.api.util.FluxToListUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RequiredArgsConstructor
@Service
public class PublisherService {
    private final ReactiveMongoTemplate mongoTemplate;
    private final PublisherRepository publisherRepository;

    public Mono<Publisher> findById(String id) {
        return publisherRepository.findById(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.PUBLISHER.getValue())));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Publisher> save(Mono<Publisher> publisherMono) {
        return publisherMono
                .flatMap(publisherRepository::save)
                .doOnError(throwable -> Mono.error(new CustomAlreadyExistException(ExceptionItemsEnum.PUBLISHER.getValue())));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Publisher> update(Mono<PublisherUpdateDTO> dto, String id) {
        return dto
                .flatMap(publisher -> publisherRepository.findByIdAndActiveTrue(id)
                        .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.PUBLISHER.getValue()))))
                .flatMap(publisherRepository::save);
        //todo relations => book collection field: name
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> delete(String id) {
        return publisherRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.PUBLISHER.getValue())))
                .map(publisher -> {
                    publisher.setActive(false);
                    return publisher;
                })
                .flatMap(publisherRepository::save)
                .map(publisher -> !publisher.isActive());
        //todo remove relations => book collection field: name
    }

    public Mono<Page<?>> getAllByPagination(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable).addCriteria(new Criteria("active").is(true));
        Flux<Publisher> publisherFlux = mongoTemplate.find(query, Publisher.class);
        Mono<Long> count = mongoTemplate.count(query, Publisher.class);
        return FluxToListUtil.toListWithPagination(publisherFlux, count, pageable);
    }
}
