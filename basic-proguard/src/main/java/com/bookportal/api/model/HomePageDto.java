package com.bookportal.api.model;

import io.swagger.annotations.ApiParam;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
public class HomePageDto {
    @ApiParam(allowableValues = "0, 1, 2")
    private String type;
    private String imageUrl;
    private String description;
    @NotBlank
    private String bookId;
}
