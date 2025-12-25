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
public class Review extends BaseEntity {
    private String review;

    private User user;

    @Override
    public String toString() {
        return "Review{" +
                "comment='" + review + '\'' +
                ", user=" + user +
                '}';
    }
}
