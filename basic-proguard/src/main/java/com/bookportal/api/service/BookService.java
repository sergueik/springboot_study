package com.bookportal.api.service;

import com.bookportal.api.entity.Author;
import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.Publisher;
import com.bookportal.api.entity.User;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.BookDTO;
import com.bookportal.api.model.BookUpdateDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.BookRepository;
import com.bookportal.api.util.FluxToListUtil;
import com.bookportal.api.util.mapper.BookMapper;
import com.bookportal.api.util.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.data.mongodb.core.aggregation.Aggregation.*;

@RequiredArgsConstructor
@Service
public class BookService {
    private final ReactiveMongoTemplate mongoTemplate;
    private final BookRepository bookRepository;
    private final PublisherService publisherService;
    private final UserService userService;
    private final AuthorService authorService;

    public Mono<Page<?>> getByPageSizeAndActiveTrueAndPublishedTrue(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable)
                .addCriteria(new Criteria("isPublished").is(true))
                .addCriteria(new Criteria("active").is(true));
        Flux<Book> bookFlux = mongoTemplate.find(query, Book.class);
        Mono<Long> count = mongoTemplate.count(query, Book.class);
        return FluxToListUtil.toListWithPagination(bookFlux, count, pageable);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> delete(String id) {
        return bookRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.BOOK.getValue())))
                .map(book -> {
                    book.setActive(false);
                    return book;
                })
                .flatMap(bookRepository::save)
                .map(book -> !book.isActive());
        //todo remove relations => quote collection where field == book.name
        //todo remove relations => userBook collection where field == book.name
        //todo remove relations => comment collection where field == book.name
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Book> save(Mono<BookDTO> bookDTOMono) {
        return bookDTOMono
                .flatMap(this::checkInputsAndInitBook)
                .map(book -> {
                    book.setIsPublished(false);
                    return book;
                })
                .flatMap(bookRepository::save);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Book> update(String id, Mono<BookUpdateDTO> bookUpdateDTO) {
        return bookRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.BOOK.getValue())))
                .flatMap(book -> bookUpdateDTO.flatMap(dto -> checkInputsAndInitBookUpdate(id, dto)));
        //todo relations => quote collection field: book.name
        //todo relations => userBook collection field: book.name
        //todo relations => comment collection field: book.name
    }

    private Mono<Book> checkInputsAndInitBook(BookDTO bookDTO) {
        List<Author> authorList = new ArrayList<>();
        for (int i = 0; i < bookDTO.getAuthorIds().length; i++) {
            authorService.findByIdAndActiveTrue((bookDTO.getAuthorIds()[i]))
                    .map(authorList::add).subscribe();
        }
        Mono<User> currentUser = userService.getCurrentUser();
        Mono<Publisher> publisherMono = publisherService.findById(bookDTO.getPublisherId());

        return currentUser
                .zipWith(publisherMono, (user, publisher) -> BookMapper.bookDTOtoBook(bookDTO, authorList, publisher, user));
    }

    private Mono<Book> checkInputsAndInitBookUpdate(String id, BookUpdateDTO bookUpdateDTO) {
        List<Author> authorList = new ArrayList<>();
        return Flux.fromIterable(Arrays.stream(bookUpdateDTO.getAuthorIds()).collect(Collectors.toList()))
                .flatMap(s -> authorService.findByIdAndActiveTrue(s).doOnNext(authorList::add))
                .collectList()
                .flatMap(authors -> publisherService.findById(bookUpdateDTO.getPublisherId())
                        .flatMap(publisher -> userService.getCurrentUser()
                                .zipWith(bookRepository.findById(id), (user, book) ->
                                        BookMapper.bookUpdateDTOtoBook(bookUpdateDTO, book, authorList, publisher, user))))
                .flatMap(bookRepository::save)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.EDITOR.getValue())));
    }

    public Mono<Book> findByIdAndActiveTrueAndIsPublishedTrue(String id) {
        return bookRepository.findByIdAndActiveTrueAndIsPublishedTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.BOOK.getValue())));
    }

    public Mono<Book> findByIdAndActiveTrue(String id) {
        return bookRepository.findByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.BOOK.getValue())));
    }

    public Mono<List<Book>> findByNameOrAuthorName(int page, int size, String text) {
        return Mono.fromCallable(() -> {
            MatchOperation operation = match(
                    new Criteria()
                            .andOperator(new Criteria("active").is(true))
                            .orOperator(
                                    Criteria.where("name").regex(".*" + text + ".*", "i"),
                                    Criteria.where("tag").regex(".*" + text + ".*", "i"),
                                    Criteria.where("authors.name").regex(".*" + text + ".*", "i"),
                                    Criteria.where("publisher.name").regex(".*" + text + ".*", "i")
                            ));
            return newAggregation(operation, skip(page), limit(size));
        }).flatMapMany(aggregation -> mongoTemplate.aggregate(
                aggregation,
                mongoTemplate.getCollectionName(Book.class),
                Book.class
        )).collectList();
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> publishBook(String id) {
        return findByIdAndActiveTrue(id).zipWith(userService.getCurrentUser(), (book, user) -> {
                    book.setIsPublished(true);
                    book.setEditor(UserMapper.userToSoftUser(user));
                    return book;
                })
                .flatMap(bookRepository::save)
                .map(book -> true);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> unPublishBook(String id) {
        return findByIdAndActiveTrue(id).zipWith(userService.getCurrentUser(), (book, user) -> {
                    book.setIsPublished(false);
                    book.setEditor(UserMapper.userToSoftUser(user));
                    return book;
                })
                .flatMap(bookRepository::save)
                .map(book -> true);
    }

    public Mono<List<Book>> findByCategoryAndPageSize(int page, int size, String categoryId) {
        Mono<Aggregation> aggregationMono = Mono.defer(() -> Mono.fromCallable(() ->
                newAggregation(match(Criteria.where("categories.id").in(categoryId)), skip(page), limit(size))));
        return aggregationMono.flatMapMany(aggregation -> mongoTemplate
                .aggregate(
                        aggregation,
                        mongoTemplate.getCollectionName(Book.class),
                        Book.class)
        ).collectList();
    }

    public Mono<Page<?>> findAllByPaginationAdmin(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable)
                .addCriteria(new Criteria("active").is(true));
        Flux<Book> bookFlux = mongoTemplate.find(query, Book.class);
        Mono<Long> count = mongoTemplate.count(query, Book.class);
        return FluxToListUtil.toListWithPagination(bookFlux, count, pageable);
    }

    public Mono<List<Book>> getLastBooks() {
        Query query = new Query()
                .limit(10)
                .with(Sort.by("createDate"))
                .addCriteria(new Criteria("isPublished").is(true))
                .addCriteria(new Criteria("active").is(true));
        return mongoTemplate.find(query, Book.class).collectList();
    }
}
