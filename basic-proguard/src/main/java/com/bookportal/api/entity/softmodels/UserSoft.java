package com.bookportal.api.entity.softmodels;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserSoft {
    private String id;
    private String name;
    private String surname;
    private String mail;
}