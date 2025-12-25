package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Data
@NoArgsConstructor
public class BookDTO {
    @NotBlank
    private String name;

    @NotNull
    @ApiModelProperty(example = "[1]")
    private String[] authorIds;

    @NotNull
    @ApiModelProperty(example = "250")
    private int page;

    @NotNull
    @ApiModelProperty(example = "1")
    private String publisherId;

    @NotNull
    @ApiModelProperty(example = "2000")
    private int year;

    @ApiModelProperty
    private String imageUrl;

    @ApiModelProperty(example = "tag")
    private String tag;
}
