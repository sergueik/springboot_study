package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.BookService;
import com.bookportal.api.service.CategoryService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/category")
@RequiredArgsConstructor
public class CategoryController {
    private final CategoryService categoryService;
    private final BookService bookService;

    @GetMapping
    @ApiOperation(value = "Get categories by pagination")
    public Mono<CustomResponse> getCategoriesByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return categoryService.getByPageSize(page, size)
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "Get categoriy by id")
    public Mono<CustomResponse> findById(@ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return categoryService.findById(id)
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}/books")
    @ApiOperation(value = "Get books by category")
    public Mono<CustomResponse> getBookByCategories(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size,
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return bookService.findByCategoryAndPageSize(page, size, id)
                .map(CustomResponse::responseOk);

    }
}
