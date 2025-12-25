package com.bookportal.api.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.entity.softmodels.UserSoft;
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
public class Quote extends BaseEntity {
    private String quote;
    private BookSoft book;
    private UserSoft user;

    @JsonProperty("favCount")
    private int count = 0;

    @Override
    public String toString() {
        return "Quote{" +
                "quote='" + quote + '}';
    }
}
