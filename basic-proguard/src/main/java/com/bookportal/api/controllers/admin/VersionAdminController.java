package com.bookportal.api.controllers.admin;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.VersionService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/admin/version")
@RequiredArgsConstructor
public class VersionAdminController {
    private final VersionService versionService;

    @PostMapping
    @ApiOperation(value = "Update version")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public Mono<CustomResponse> updateVersion(
            @ApiParam(allowableValues = "0,1") @RequestParam(name = "environment") String environment,
            @ApiParam @RequestParam(name = "minVersion") String minVersion,
            @ApiParam @RequestParam(name = "marketVersion") String marketVersion) {
        return versionService.updateVersion(minVersion, marketVersion, environment)
                .map(CustomResponse::responseOk);
    }
}
