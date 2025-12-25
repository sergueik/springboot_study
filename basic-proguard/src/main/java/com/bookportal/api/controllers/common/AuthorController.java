package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.AuthorService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/author")
@RequiredArgsConstructor
public class AuthorController {
    private final AuthorService authorService;

    @GetMapping
    @ApiOperation(value = "Get publishers by pagination")
    public Mono<CustomResponse> getAuthorsByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return authorService.getAllByPagination(page, size).map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "Get publisher by id")
    public Mono<CustomResponse> getAuthorById(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return authorService.findByIdAndActiveTrue(id)
                .map(CustomResponse::responseOk);
    }
}
