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
public class Notification extends BaseEntity {
    private String subject;
    private String comment;
    private User user;

    @Override
    public String toString() {
        return "Notification{" +
                "subject='" + subject + '\'' +
                ", comment='" + comment + '\'' +
                ", user=" + user +
                '}';
    }
}
