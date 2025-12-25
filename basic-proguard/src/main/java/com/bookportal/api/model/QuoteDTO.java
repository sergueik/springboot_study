package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Data
public class QuoteDTO {
    @NotBlank
    private String quote;

    @NotNull
    private String bookId;
}
