package com.bookportal.api.service;

import com.bookportal.api.entity.Author;
import com.bookportal.api.exception.BusinessException;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.AuthorDTO;
import com.bookportal.api.model.AuthorUpdateDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.AuthorRepository;
import com.bookportal.api.util.FluxToListUtil;
import com.bookportal.api.util.mapper.AuthorMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
public class AuthorService {
    private final AuthorRepository authorRepository;
    private final ReactiveMongoTemplate reactiveMongoTemplate;
    private final AuthorMapper authorMapper;

    public Mono<Author> findByIdAndActiveTrue(String id) {
        return authorRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(() -> {
                    return new CustomNotFoundException(ExceptionItemsEnum.AUTHOR.getValue());
                }));

    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Author> saveAuthor(Mono<AuthorDTO> authorDTO) {
        return authorDTO
                .flatMap(authorMapper::authorDTOtoAuthor)
                .flatMap(authorRepository::save)
                .doOnError(ex -> Mono.error(new BusinessException(ex.getMessage())));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Author> deleteAuthor(String id) {
        return authorRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(() -> {
                    return new CustomNotFoundException(ExceptionItemsEnum.AUTHOR.getValue());
                }))
                .map(author -> {
                    author.setActive(false);
                    return author;
                })
                .flatMap(authorRepository::save)
                .doOnError(ex -> {
                    Mono.error(new BusinessException(ex.getMessage()));
                });
        //todo relations => book collection field: name
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Author> updateAuthor(Mono<AuthorUpdateDTO> dtoMono, String id) {
        return dtoMono
                .flatMap(authorUpdateDTO -> findByIdAndActiveTrue(id)
                        .map(author -> authorMapper.authorUpdateDTOtoAuthor(authorUpdateDTO, author)))
                .flatMap(authorRepository::save);
        //todo relations => book collection field: name
    }

    public Mono<Page<?>> getAllByPagination(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable).addCriteria(new Criteria("active").is(true));
        Flux<Author> authorFlux = reactiveMongoTemplate.find(query, Author.class);
        Mono<Long> count = reactiveMongoTemplate.count(query, Author.class);
        return FluxToListUtil.toListWithPagination(authorFlux, count, pageable);
    }

}
