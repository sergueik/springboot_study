package com.bookportal.api.controllers.common;

import com.bookportal.api.model.CommentDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.CommentService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/comment")
@RequiredArgsConstructor
public class CommentController {
    private final CommentService commentService;

    @GetMapping
    @ApiOperation(value = "Get comments")
    public Mono<CustomResponse> getCommentsByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return commentService.findAllByPaginationAndActiveTrue(PageRequest.of(page, size))
                .map(CustomResponse::responseOk);
    }

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save comment, updates if already shared a comment on same book")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> saveComment(@Valid @RequestBody Mono<CommentDTO> dtoMono) {
        return dtoMono
                .flatMap(commentService::save)
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/book/{id}")
    @ApiOperation(value = "Get comments by page")
    public Mono<CustomResponse> getCommentsByBookAndPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size,
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String bookId) {
        return commentService.findCommentsByBook(PageRequest.of(page, size), bookId)
                .map(CustomResponse::responseOk);
    }
}
