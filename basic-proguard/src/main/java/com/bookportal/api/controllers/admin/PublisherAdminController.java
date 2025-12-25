package com.bookportal.api.controllers.admin;

import com.bookportal.api.model.PublisherDTO;
import com.bookportal.api.model.PublisherUpdateDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.PublisherService;
import com.bookportal.api.util.mapper.PublisherMapper;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/admin/publisher")
@RequiredArgsConstructor
public class PublisherAdminController {
    private final PublisherService publisherService;

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save publisher")
    public Mono<CustomResponse> savePublisher(@Valid @RequestBody Mono<PublisherDTO> dtoMono) {
        return dtoMono
                .flatMap(publisherDTO -> publisherService.save(PublisherMapper.dtoToPublisher(publisherDTO)))
                .map(CustomResponse::responseCreated);
    }

    @PutMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Update publisher")
    public Mono<CustomResponse> updatePublisher(
            @ApiParam(defaultValue = "1", required = true) @PathVariable("id") String id,
            @ApiParam(required = true) @Valid @RequestBody Mono<PublisherUpdateDTO> dto) {
        return dto
                .flatMap(publisherUpdateDTO -> publisherService.update(dto,id))
                .map(CustomResponse::responseOk);

    }

    @DeleteMapping("/{id}")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete publisher")
    public Mono<CustomResponse> deletePublisher(@PathVariable("id") String id) {
        return publisherService.delete(id)
                .map(CustomResponse::responseOk);
    }
}
