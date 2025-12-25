package com.bookportal.api.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class PasswordReset extends BaseEntity {
    private String secretKey;
    private Date validity;
    private User user;

    @Override
    public String toString() {
        return "PasswordReset{" +
                "secretKey='" + secretKey + '\'' +
                ", validity=" + validity +
                ", user=" + user +
                '}';
    }
}
