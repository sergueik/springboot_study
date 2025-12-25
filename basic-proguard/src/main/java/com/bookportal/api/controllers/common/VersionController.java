package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.VersionService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/version")
@RequiredArgsConstructor
public class VersionController {
    private final VersionService versionService;

    @GetMapping
    @ApiOperation(value = "get required platform versions")
    public Mono<CustomResponse> getVersion(
            @ApiParam(allowableValues = "0,1") @RequestParam(name = "environment") String environment,
            @ApiParam @RequestParam(name = "version") String version) {
        return versionService.checkVersion(version, environment)
                .map(CustomResponse::responseOk);
    }
}
