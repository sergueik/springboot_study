package com.bookportal.api.service;

import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.User;
import com.bookportal.api.entity.Vote;
import com.bookportal.api.repository.VoteRepository;
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

@Service
@RequiredArgsConstructor
public class VoteService {
    private final ReactiveMongoTemplate mongoTemplate;
    private final VoteRepository voteRepository;
    private final UserService userService;
    private final BookService bookService;

    public Mono<Page<?>> findVoteByUser(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return userService.getCurrentUser()
                .flatMap(user -> {
                    Query query = new Query()
                            .with(pageable)
                            .addCriteria(new Criteria("userId").is(user.getId()));
                    Flux<Vote> voteFlux = mongoTemplate.find(query, Vote.class);
                    Mono<Long> count = mongoTemplate.count(query, Vote.class);
                    return FluxToListUtil.toListWithPagination(voteFlux, count, pageable);
                });
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Vote> voteBook(String bookId, int v) {
        Mono<Book> bookMono = bookService.findByIdAndActiveTrueAndIsPublishedTrue(bookId);
        return userService.getCurrentUser()
                .zipWith(bookMono, (user, book) -> voteRepository.findByUserIdAndBookId(user.getId(), book.getId())
                        .switchIfEmpty(initIfEmpty(book, v, user))
                        .map(vote -> {
                            vote.setVote(v);
                            return vote;
                        })
                        .flatMap(voteRepository::save))
                .flatMap(voteMono -> voteMono.map(vote -> vote));
    }

    private Mono<Vote> initIfEmpty(Book book, int v, User user) {
        return (Mono.fromSupplier(() -> {
            Vote vote = new Vote();
            vote.setVote(v);
            vote.setBookId(book.getId());
            vote.setUserId(user.getId());
            return vote;
        }));
    }
}
