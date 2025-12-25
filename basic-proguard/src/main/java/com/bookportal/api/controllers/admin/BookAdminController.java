package com.bookportal.api.controllers.admin;

import com.bookportal.api.model.BookDTO;
import com.bookportal.api.model.BookUpdateDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.BookService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/admin/book")
@RequiredArgsConstructor
public class BookAdminController {
    private final BookService bookService;

    @GetMapping
    @ApiOperation(value = "Get books by pagination")
    public Mono<CustomResponse> getAllBooks(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return bookService.findAllByPaginationAdmin(page, size)
                .map(CustomResponse::responseOk);
    }

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save book")
    public Mono<CustomResponse> saveBook(@Valid @RequestBody Mono<BookDTO> bookDTO) {
        return bookService.save(bookDTO)
                .map(CustomResponse::responseCreated);
    }

    @PutMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Update book")
    public Mono<CustomResponse> updateBook(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id,
            @Valid @RequestBody Mono<BookUpdateDTO> bookUpdateDTO) {
        return bookService.update(id, bookUpdateDTO)
                .map(CustomResponse::responseOk);
    }

    @DeleteMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete book")
    public Mono<CustomResponse> deleteBook(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return bookService.delete(id)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/{id}/publish")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Publish book")
    public Mono<CustomResponse> publishBook(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String bookId) {
        return bookService.publishBook(bookId)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/{id}/unPublish")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Unpublish book")
    public Mono<CustomResponse> unPublishBook(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String bookId) {
        return bookService.unPublishBook(bookId)
                .map(CustomResponse::responseOk);
    }
}
