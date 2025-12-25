package com.bookportal.api.util.mapper;


import com.bookportal.api.entity.Category;
import com.bookportal.api.model.CategoryDTO;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Component
public class CategoryMapper {
    public static Mono<Category> dtoToCategory(CategoryDTO categoryDTO) {
        return Mono.fromSupplier(() -> {
            Category category = new Category();
            category.setName(categoryDTO.getName());
            category.setActive(true);
            return category;
        });
    }
}
