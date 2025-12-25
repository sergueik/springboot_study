package com.bookportal.api.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.ToString;

@Data
@AllArgsConstructor
@ToString
public class RequestFilterLogDTO {
    private String username;
    private String url;
    private String requestType;
    private String message;
    private String ip_address;
}
