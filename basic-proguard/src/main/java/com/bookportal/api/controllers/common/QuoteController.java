package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.FavouriteService;
import com.bookportal.api.service.QuoteService;
import com.bookportal.api.service.RandomQuoteService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/quote")
@RequiredArgsConstructor
public class QuoteController {
    private final QuoteService quoteService;
    private final FavouriteService favouriteService;
    private final RandomQuoteService randomQuoteService;

    @GetMapping
    @ApiOperation(value = "Get quotes")
    public Mono<CustomResponse> getQuotesByPageSize(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return quoteService.getByPageSize(page, size).map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "get quote by id")
    public Mono<CustomResponse> findQuoteById(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return quoteService.findByIdAndActiveTrue(id).map(CustomResponse::responseOk);
    }

    @GetMapping("/random")
    @ApiOperation(value = "get random quote")
    public Mono<CustomResponse> getRandomQuote() {
        return randomQuoteService.getRandomQuote().map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}/fav")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "add quote to users favourite list")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> addQuoteToFavourite(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return favouriteService.addQuoteToFavourite(id).map(CustomResponse::responseOk);
    }

    @GetMapping("/{id}/favourites")
    @ApiOperation(value = "get users who added quote to their fav list")
    public Mono<CustomResponse> findFavorites(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return favouriteService.findFavouritesByQuote(id).map(CustomResponse::responseOk);
    }
}
