package com.bookportal.api.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;

@Data
public class GuestDTO {
    @NotBlank
    @ApiModelProperty(example = "androidId")
    private String androidID;
}
