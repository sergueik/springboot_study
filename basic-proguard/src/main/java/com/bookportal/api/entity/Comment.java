package com.bookportal.api.entity;

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
public class Comment extends BaseEntityInactive {
    private String comment;
    private BookSoft book;
    private UserSoft user;

    @Override
    public String toString() {
        return "Comment{" +
                "comment='" + comment + '\'' +
                ", user=" + user +
                '}';
    }
}
