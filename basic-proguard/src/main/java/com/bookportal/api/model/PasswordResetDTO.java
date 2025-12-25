package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Data
public class PasswordResetDTO {
    @NotBlank
    @Email
    private String email;

    @NotBlank
    private String key;

    @NotBlank
    @Size(min = 6, max = 255)
    private String newPass;
}
