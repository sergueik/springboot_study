package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;

@Data
public class AuthorUpdateDTO {
    @NotBlank
    @ApiModelProperty
    private String name;

    @NotBlank
    @ApiModelProperty
    private String about;

    @ApiModelProperty
    private String imageUrl;
}
