package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Data
@NoArgsConstructor
public class SocialDTO {
    @NotBlank
    @Email
    private String mail;
    private String name;
    private String surname;
    @NotNull
    private String googleId;
    @NotNull
    private String facebookId;
    private String ppUrl;
}
