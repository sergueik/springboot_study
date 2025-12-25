package com.bookportal.api.controllers.common;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.HomePageService;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/home")
@RequiredArgsConstructor
public class HomePageController {
    private final HomePageService homePageService;

    @GetMapping
    @ApiOperation(value = "Homepage data", response = Page.class)
    public Mono<CustomResponse> getHomeDate() {
        return homePageService.homePageResponse()
                .map(CustomResponse::responseOk);
    }
}
