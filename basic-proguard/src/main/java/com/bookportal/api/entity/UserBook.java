package com.bookportal.api.entity;

import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.entity.softmodels.UserSoft;
import com.bookportal.api.model.enums.UserBookEnum;
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
public class UserBook extends BaseEntity {
    private UserSoft user;
    private BookSoft book;
    private UserBookEnum type;
}
