package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Data
public class CommentDTO {
    @NotBlank
    @ApiModelProperty
    private String comment;

    @NotNull
    @ApiModelProperty(example = "1")
    private String bookId;
}
