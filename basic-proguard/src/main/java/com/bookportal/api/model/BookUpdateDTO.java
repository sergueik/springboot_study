package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Data
public class BookUpdateDTO {
    @NotBlank
    private String name;

    @NotNull
    private String[] authorIds;

    @NotNull
    @ApiModelProperty(example = "100")
    private int page;

    @NotNull
    @ApiModelProperty(example = "3")
    private String publisherId;

    @NotNull
    @ApiModelProperty(example = "2010")
    private int year;

    private String imageUrl;

    @ApiModelProperty(example = "tag")
    private String tag;

    @NotNull
    @ApiModelProperty(example = "true")
    private Boolean isPublished;
}
