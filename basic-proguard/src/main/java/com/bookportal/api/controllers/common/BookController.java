package com.bookportal.api.controllers.common;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.BookService;
import com.bookportal.api.service.UserBookService;
import com.bookportal.api.service.VoteService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/book")
@RequiredArgsConstructor
public class BookController {
    private final BookService bookService;
    private final VoteService voteService;
    private final UserBookService userBookService;
    private final EnvironmentVariables env;

    @GetMapping
    @ApiOperation(value = "Get books by pagination")
    public Mono<CustomResponse> getBooksByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return bookService.getByPageSizeAndActiveTrueAndPublishedTrue(page, size).map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "Get book by id")
    public Mono<CustomResponse> findBookById(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(id)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/{id}/vote")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Vote book [1,5]")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> voteBook(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String bookId,
            @ApiParam(defaultValue = "1", allowableValues = "1,2,3,4,5", required = true) @RequestParam("vote") int vote) {
        if (vote < 1) {
            throw new ResponseStatusException(HttpStatus.NOT_ACCEPTABLE, env.lessThanOne());
        }
        if (vote > 5) {
            throw new ResponseStatusException(HttpStatus.NOT_ACCEPTABLE, env.greaterThanFive());
        }
        return voteService.voteBook(bookId, vote)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/search")
    @ApiOperation(value = "Search book by title or author name")
    public Mono<CustomResponse> findBookByNameOrAuthorName(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size,
            @ApiParam(defaultValue = "victor hugo", required = true) @RequestParam(name = "text") String text) {
        return bookService.findByNameOrAuthorName(page, size, text)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/{id}/read")
    @ApiOperation(value = "Add book to will read / have read list")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> willRead(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String bookId,
            @ApiParam(allowableValues = "0,1") @RequestParam("type") String type) {
        return userBookService.save(bookId, type)
                .map(CustomResponse::responseOk);
    }
}
