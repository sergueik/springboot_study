package com.bookportal.api.entity.softmodels;

import com.bookportal.api.entity.BaseEntity;
import com.bookportal.api.entity.Category;
import com.bookportal.api.entity.Publisher;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class BookNoAuthor extends BaseEntity {
    private String name;
    private List<Category> categories;
    private int page;
    private Publisher publisher;
    private int year;
    private String imageUrl;
    private String tag;
    private Boolean isPublished;
}
