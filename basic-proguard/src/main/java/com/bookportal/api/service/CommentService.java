package com.bookportal.api.service;

import com.bookportal.api.entity.Comment;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.CommentDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.CommentRepository;
import com.bookportal.api.util.FluxToListUtil;
import com.bookportal.api.util.mapper.BookMapper;
import com.bookportal.api.util.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.bson.types.ObjectId;
import org.springframework.data.domain.Page;
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
public class CommentService {
    private final BookService bookService;
    private final UserService userService;
    private final CommentRepository commentRepository;
    private final ReactiveMongoTemplate reactiveMongoTemplate;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Comment> save(CommentDTO commentDTO) {
        return getOrEmpty(commentDTO.getBookId())
                .map(comment -> {
                    comment.setComment(commentDTO.getComment());
                    return comment;
                }).switchIfEmpty(initComment(commentDTO.getBookId(), commentDTO.getComment()))
                .flatMap(commentRepository::save);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Comment> approve(String id) {
        return commentRepository.findById(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.COMMENT.getValue())))
                .map(comment -> {
                    comment.setActive(true);
                    return comment;
                })
                .flatMap(commentRepository::save);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> setInactive(String id) {
        return commentRepository.findById(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.COMMENT.getValue())))
                .flatMap(commentRepository::delete)
                .thenReturn(true);
    }

    public Mono<Page<?>> findAllByPaginationAndActiveTrue(Pageable pageable) {
        Query query = new Query()
                .with(pageable)
                .addCriteria(new Criteria("active").is(true));
        Flux<Comment> queryFlux = reactiveMongoTemplate.find(query, Comment.class);
        Mono<Long> count = reactiveMongoTemplate.count(query, Comment.class);
        return FluxToListUtil.toListWithPagination(queryFlux, count, pageable);
    }

    public Mono<Page<?>> findAllByPagination(Pageable pageable) {
        Query query = new Query().with(pageable);
        Flux<Comment> queryFlux = reactiveMongoTemplate.find(query, Comment.class);
        Mono<Long> count = reactiveMongoTemplate.count(query, Comment.class);
        return FluxToListUtil.toListWithPagination(queryFlux, count, pageable);
    }

    public Mono<Page<?>> findCommentsByBook(Pageable pageable, String bookId) {
        Query query = new Query().with(pageable)
                .addCriteria(new Criteria("active").is(true))
                .addCriteria(new Criteria("book._id").is(new ObjectId(bookId)));
        Flux<Comment> queryFlux = reactiveMongoTemplate.find(query, Comment.class);
        Mono<Long> count = reactiveMongoTemplate.count(query, Comment.class);
        return FluxToListUtil.toListWithPagination(queryFlux, count, pageable);
    }

    private Mono<Comment> getOrEmpty(String bookId) {
        return userService.getCurrentUser()
                .flatMap(user -> {
                    Query query = new Query()
                            .addCriteria(new Criteria("book._id").is(new ObjectId(bookId)))
                            .addCriteria(new Criteria("user._id").is(new ObjectId(user.getId())));
                    return reactiveMongoTemplate.findOne(query, Comment.class);
                }).switchIfEmpty(Mono.empty());
    }

    private Mono<Comment> initComment(String bookId, String comment) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(bookId)
                .zipWith(userService.getCurrentUser(), (book, user) -> {
                    Comment commentObj = new Comment();
                    commentObj.setUser(UserMapper.userToSoftUser(user));
                    commentObj.setBook(BookMapper.bookToSoft(book));
                    commentObj.setComment(comment);
                    return commentObj;
                });
    }
}
