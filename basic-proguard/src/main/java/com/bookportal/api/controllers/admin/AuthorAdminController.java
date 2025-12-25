package com.bookportal.api.controllers.admin;

import com.bookportal.api.model.AuthorDTO;
import com.bookportal.api.model.AuthorUpdateDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.AuthorService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/admin/author")
@RequiredArgsConstructor
public class AuthorAdminController {
    private final AuthorService authorService;

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save author")
    public Mono<CustomResponse> saveAuthor(@Valid @RequestBody Mono<AuthorDTO> authorDTO) {
        return authorService.saveAuthor(authorDTO)
                .map(CustomResponse::responseCreated);
    }

    @PutMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Update author")
    public Mono<CustomResponse> updateAuthor(
            @ApiParam(defaultValue = "1") @PathVariable("id") String id,
            @Valid @RequestBody Mono<AuthorUpdateDTO> authorUpdateDTO) {
        return authorService.updateAuthor(authorUpdateDTO,id)
                .map(CustomResponse::responseOk);
    }

    @DeleteMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete author")
    public Mono<CustomResponse> deleteAuthor(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id) {
        return authorService.deleteAuthor(id)
                .map(CustomResponse::responseOk);
    }

}
