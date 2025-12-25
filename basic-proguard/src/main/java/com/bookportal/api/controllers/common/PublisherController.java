package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.PublisherService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/publisher")
@RequiredArgsConstructor
public class PublisherController {
    private final PublisherService publisherService;

    @GetMapping
    @ApiOperation(value = "Get publishers")
    public Mono<CustomResponse> getPublishersByPagination(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return publisherService.getAllByPagination(page, size)
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "get publisher by id")
    public Mono<CustomResponse> findPublisherById(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return publisherService.findById(id).map(CustomResponse::responseOk);
    }
}
