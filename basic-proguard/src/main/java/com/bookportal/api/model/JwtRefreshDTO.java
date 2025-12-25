package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;

@Data
public class JwtRefreshDTO {
    @NotBlank
    private String mail;

    @NotBlank
    private String token;
}
