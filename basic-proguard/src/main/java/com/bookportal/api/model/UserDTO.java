package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Data
@NoArgsConstructor
public class UserDTO {
    @NotBlank
    @Email
    private String mail;

    @NotBlank
    @Size(min = 6)
    private String password;

}