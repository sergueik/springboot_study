package com.bookportal.api.entity;

import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.model.enums.HomePageEnum;
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
public class HomePage extends BaseEntity {
    private HomePageEnum type;
    private BookSoft book;
    private String imageUrl;
    private String description;

    @Override
    public String toString() {
        return "HomePage{" +
                "type=" + type +
                ", book=" + book +
                '}';
    }
}
