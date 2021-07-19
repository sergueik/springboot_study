package org.example.ua.dto.response;

import lombok.Data;

import java.util.Date;

@Data
public class ErrorResponse {

    private final Date timestamp;
    private final String message;
    private final String details;
}
