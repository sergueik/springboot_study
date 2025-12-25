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
public class Vote extends BaseEntity {
    private String bookId;
    private String userId;
    private int vote;

    @Override
    public String toString() {
        return "Vote{" +
                "bookId=" + bookId +
                ", userId=" + userId +
                ", vote=" + vote +
                '}';
    }

}
