package com.bookportal.api.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class Author extends BaseEntity {
    private String name;
    private String about;
    private String imageUrl;

    @Override
    public String toString() {
        return "Author{" +
                "name='" + name + '\'' +
                "about='" + about + '\'' +
                "imageUrl='" + imageUrl + '\'' +
                '}';
    }
}
