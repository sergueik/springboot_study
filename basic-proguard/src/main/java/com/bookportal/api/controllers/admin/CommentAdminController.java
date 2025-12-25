package com.bookportal.api.controllers.admin;

import com.bookportal.api.entity.BaseEntityInactive;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.CommentService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/admin/comment")
@RequiredArgsConstructor
public class CommentAdminController {
    private final CommentService commentService;

    @GetMapping
    @ApiOperation(value = "Get comments by pagination")
    public Mono<CustomResponse> getCommentsByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return commentService.findAllByPagination(PageRequest.of(page, size))
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/{id}/approve")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Approve comment")
    public Mono<CustomResponse> approveComment(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return commentService.approve(id)
                .doOnError(throwable -> Mono.error(new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "hata oldu")))
                .map(BaseEntityInactive::isActive)
                .map(CustomResponse::responseOk);
    }

    @DeleteMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete comment for good")
    public Mono<CustomResponse> refuseComment(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return commentService.setInactive(id)
                .doOnError(throwable -> Mono.error(new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "hata oldu")))
                .map(CustomResponse::responseOk);
    }
}
