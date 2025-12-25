package com.bookportal.api.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.bookportal.api.entity.softmodels.UserSoft;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class Book extends BaseEntity {
    private String name;
    private String imageUrl;
    private String tag;
    private int page;
    private int year;
    private Boolean isPublished;
    private Publisher publisher;
    private List<Author> authors;
    private List<Category> categories;

    @JsonIgnore
    private UserSoft user;

    @JsonIgnore
    private UserSoft editor;


    @Override
    public String toString() {
        return "Book{" +
                "name='" + name + '\'' +
                ", page=" + page +
                ", year=" + year +
                ", imageUrl='" + imageUrl + '\'' +
                ", tag='" + tag + '\'' +
                ", isPublished=" + isPublished +
                '}';
    }
}
