package com.bookportal.api.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class User extends BaseEntity {
    private String name;
    private String surname;
    private String ppUrl;
    private ArrayList<Role> roles;
    private Boolean social;

    @Indexed(unique = true)
    private String mail;

    @JsonIgnore
    private String password;

    @Indexed(unique = true)
    private String googleId;

    @Indexed(unique = true)
    private String facebookId;

    @Override
    public String toString() {
        return "User{" +
                "mail='" + mail + '\'' +
                '}';
    }
}