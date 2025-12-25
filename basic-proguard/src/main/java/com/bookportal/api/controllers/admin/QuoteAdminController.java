package com.bookportal.api.controllers.admin;

import com.bookportal.api.model.QuoteDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.QuoteService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/admin/quote")
@RequiredArgsConstructor
public class QuoteAdminController {
    private final QuoteService quoteService;

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save quote")
    public Mono<CustomResponse> saveQuote(@Valid @RequestBody Mono<QuoteDTO> dto) {
        return dto
                .flatMap(quoteDTO -> quoteService.save(quoteDTO.getQuote(), quoteDTO.getBookId()))
                .map(CustomResponse::responseOk);
    }

    @DeleteMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete quote")
    public Mono<CustomResponse> deleteQuoteById(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return quoteService.delete(id)
                .map(CustomResponse::responseOk);
    }
}
