package com.bookportal.api.service;

import com.bookportal.api.entity.Favourite;
import com.bookportal.api.entity.softmodels.UserSoft;
import com.bookportal.api.repository.FavouriteRepository;
import com.bookportal.api.util.FluxToListUtil;
import com.bookportal.api.util.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.bson.types.ObjectId;
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

import java.util.List;

@Service
@RequiredArgsConstructor
public class FavouriteService {
    private final ReactiveMongoTemplate reactiveMongoTemplate;
    private final FavouriteRepository favouriteRepository;
    private final UserService userService;
    private final QuoteService quoteService;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Favourite> addQuoteToFavourite(String quoteId) {
        return userService.getCurrentUser()
                .flatMap(user -> {
                    Query query = new Query()
                            .addCriteria(new Criteria("quote._id").is(new ObjectId(quoteId)))
                            .addCriteria(new Criteria("user._id").is(new ObjectId(user.getId())));
                    return reactiveMongoTemplate.findOne(query, Favourite.class);
                })
                .switchIfEmpty(Mono.empty())
                .map(favourite -> {
                    favourite.setActive(!favourite.isActive());
                    return favourite;
                })
                .switchIfEmpty(initFavObj(quoteId))
                .flatMap(favourite -> {
                    if (favourite.isActive()) {
                        return quoteService.increaseQuoteCount(Mono.just(favourite.getQuote()))
                                .flatMap(quote -> favouriteRepository.save(favourite));
                    } else {
                        return quoteService.decreaseQuoteCount(Mono.just(favourite.getQuote()))
                                .flatMap(quote -> favouriteRepository.save(favourite));
                    }
                });

    }

    public Mono<List<UserSoft>> findFavouritesByQuote(String id) {
        return favouriteRepository.findAllByQuoteIdAndActiveTrue(id)
                .map(Favourite::getUser)
                .collectList();
    }

    public Mono<Page<?>> findFavouritesByUser(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return userService.getCurrentUser()
                .flatMap(user -> {
                    Query query = new Query().with(pageable)
                            .addCriteria(new Criteria("active").is(true))
                            .addCriteria(new Criteria("user._id").is(new ObjectId(user.getId())));

                    Flux<Favourite> favouriteFlux = reactiveMongoTemplate.find(query, Favourite.class);
                    Mono<Long> count = reactiveMongoTemplate.count(query, Favourite.class);
                    return FluxToListUtil.toListWithPagination(favouriteFlux, count, pageable);
                });
    }

    private Mono<Favourite> initFavObj(String quoteId) {
        return quoteService.findByIdAndActiveTrue(quoteId)
                .zipWith(userService.getCurrentUser(), (quote, user) -> {
                    Favourite fav = new Favourite();
                    fav.setUser(UserMapper.userToSoftUser(user));
                    fav.setActive(true);
                    fav.setQuote(quote);
                    return fav;
                });
    }

}
