package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Data
@NoArgsConstructor
public class UserRegisterDTO {
    @NotBlank
    @Email
    private String mail;

    @NotBlank
    @Size(min = 6, max = 255)
    private String password;

    @NotBlank
    private String name;

    @NotBlank
    private String surname;
}