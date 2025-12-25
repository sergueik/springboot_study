package com.bookportal.api.controllers.admin;

import com.bookportal.api.entity.Category;
import com.bookportal.api.model.CategoryDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.CategoryService;
import com.bookportal.api.util.mapper.CategoryMapper;
import io.swagger.annotations.ApiOperation;
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
@RequestMapping("/api/v1/admin/category")
@RequiredArgsConstructor
public class CategoryAdminController {
    private final CategoryService categoryService;

    @PostMapping
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Save category")
    public Mono<CustomResponse> save(@Valid @RequestBody CategoryDTO categoryDTO) {
        Mono<Category> categoryMono = CategoryMapper.dtoToCategory(categoryDTO);
        return categoryService.save(categoryMono).map(CustomResponse::responseOk);
    }
}
