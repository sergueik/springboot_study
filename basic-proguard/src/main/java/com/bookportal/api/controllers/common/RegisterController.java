package com.bookportal.api.controllers.common;

import com.bookportal.api.model.UserRegisterDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.RegisterService;
import com.bookportal.api.util.mapper.UserMapper;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/register")
@RequiredArgsConstructor
public class RegisterController {
    private final RegisterService registerService;

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "register")
    public Mono<CustomResponse> createUser(
            @ApiParam(required = true)
            @Valid @RequestBody Mono<UserRegisterDTO> userRegisterDTO) {
        return userRegisterDTO
                .map(UserMapper::userRegisterDTOtoUser)
                .flatMap(registerService::createUser)
                .map(CustomResponse::responseCreated);
    }
}