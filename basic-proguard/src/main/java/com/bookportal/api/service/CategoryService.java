package com.bookportal.api.service;

import com.bookportal.api.entity.Category;
import com.bookportal.api.exception.CustomAlreadyExistException;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.CategoryRepository;
import com.bookportal.api.util.FluxToListUtil;

import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
//the lombok.extern.slf4j.Sld4j does not work.
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Locale;

@Slf4j
@Service
@RequiredArgsConstructor
public class CategoryService {
	private final Logger log = LoggerFactory.getLogger(CategoryService.class);

    private final CategoryRepository categoryRepository;
    private final ReactiveMongoTemplate mongoTemplate;

    public Mono<Page<?>> getByPageSize(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable).addCriteria(new Criteria("active").is(true));
        Flux<Category> publisherFlux = mongoTemplate.find(query, Category.class);
        Mono<Long> count = mongoTemplate.count(query, Category.class);
        return FluxToListUtil.toListWithPagination(publisherFlux, count, pageable);
    }

    public Mono<Category> findById(String id) {
        return categoryRepository.findById(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.CATEGORY.getValue())));
    }

    public Mono<Category> save(Mono<Category> categoryMono) {
        return categoryMono
                .map(category -> {
                    category.setName(firstLetterUppercase(category.getName()));
                    return category;
                })
                .flatMap(categoryRepository::save)
                .doOnError(throwable -> {
                    log.error(throwable.getMessage());
                    Mono.error(new CustomAlreadyExistException(ExceptionItemsEnum.CATEGORY.getValue()));
                });
    }

    private String firstLetterUppercase(String category) {
        category = category.toLowerCase(Locale.ROOT);
        String str = category.trim();
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
