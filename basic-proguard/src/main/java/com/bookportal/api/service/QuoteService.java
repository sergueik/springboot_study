package com.bookportal.api.service;

import com.bookportal.api.entity.Quote;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.QuoteRepository;
import com.bookportal.api.util.FluxToListUtil;
import com.bookportal.api.util.mapper.BookMapper;
import com.bookportal.api.util.mapper.UserMapper;
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
public class QuoteService {
    private final QuoteRepository quoteRepository;
    private final UserService userService;
    private final BookService bookService;
    private final ReactiveMongoTemplate mongoTemplate;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Quote> save(String newQuote, String bookId) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(bookId)
                .flatMap(book -> userService.getCurrentUser()
                        .map(user -> {
                            Quote quote = new Quote();
                            quote.setQuote(newQuote);
                            quote.setUser(UserMapper.userToSoftUser(user));
                            quote.setBook(BookMapper.bookToSoft(book));
                            quote.setActive(true);
                            return quote;
                        })).flatMap(quoteRepository::save);
    }

    public Mono<Page<?>> getByPageSize(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Query query = new Query().with(pageable).addCriteria(new Criteria("active").is(true));
        Flux<Quote> quoteFlux = mongoTemplate.find(query, Quote.class);
        Mono<Long> count = mongoTemplate.count(query, Quote.class);
        return FluxToListUtil.toListWithPagination(quoteFlux, count, pageable);
    }

    public Mono<Quote> findByIdAndActiveTrue(String id) {
        return quoteRepository.findQuoteByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.QUOTE.getValue())));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> delete(String id) {
        return quoteRepository.findQuoteByIdAndActiveTrue(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.QUOTE.getValue())))
                .map(quote -> {
                    quote.setActive(false);
                    return quote;
                })
                .flatMap(quoteRepository::save)
                .thenReturn(true);
    }

    public Mono<Quote> increaseQuoteCount(Mono<Quote> quote) {
        return quote.map(quote1 -> {
            quote1.setCount(quote1.getCount() + 1);
            return quote1;
        }).flatMap(quoteRepository::save);
    }

    public Mono<Quote> decreaseQuoteCount(Mono<Quote> quote) {
        return quote.map(quote1 -> {
            quote1.setCount(quote1.getCount() - 1);
            return quote1;
        }).flatMap(quoteRepository::save);
    }
}
