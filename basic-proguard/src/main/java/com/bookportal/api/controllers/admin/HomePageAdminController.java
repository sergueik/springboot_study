package com.bookportal.api.controllers.admin;

import com.bookportal.api.entity.HomePage;
import com.bookportal.api.model.HomePageDto;
import com.bookportal.api.model.enums.HomePageEnum;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.BookService;
import com.bookportal.api.service.HomePageService;
import com.bookportal.api.util.mapper.BookMapper;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/v1/admin/home")
@RequiredArgsConstructor
public class HomePageAdminController {
    private final HomePageService homePageService;
    private final BookService bookService;

    @PostMapping("/save")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Add homepage data", response = Page.class)
    public Mono<CustomResponse> saveHomeDate(@Valid @RequestBody HomePageDto dto) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(dto.getBookId())
                .map(book -> {
                    HomePage homePage = new HomePage();
                    homePage.setBook(BookMapper.bookToSoft(book));
                    homePage.setType(HomePageEnum.findByType(dto.getType()));
                    homePage.setDescription(dto.getDescription());
                    homePage.setImageUrl(dto.getImageUrl());
                    return homePage;
                })
                .flatMap(homePageService::save)
                .map(CustomResponse::responseCreated);
    }

    @DeleteMapping("/delete")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Delete homepage data", response = Boolean.class)
    public Mono<CustomResponse> deleteHomeData(
            @ApiParam(required = true, allowableValues = "0, 1, 2")
            @RequestParam(name = "type") String type,
            @ApiParam(defaultValue = "1", required = true) @RequestParam(name = "bookId") String bookId) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(bookId)
                .map(book -> {
                    HomePage homePage = new HomePage();
                    homePage.setBook(BookMapper.bookToSoft(book));
                    homePage.setType(HomePageEnum.findByType(type));
                    return Mono.just(homePage);
                })
                .flatMap(homePageService::delete)
                .map(CustomResponse::responseOk);
    }

}
