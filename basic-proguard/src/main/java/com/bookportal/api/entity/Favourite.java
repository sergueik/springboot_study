package com.bookportal.api.entity;

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
public class Favourite extends BaseEntity {
    private Quote quote;
    private UserSoft user;

    @Override
    public String toString() {
        return "Favourite{" +
                "quote=" + quote +
                ", user=" + user +
                '}';
    }
}
