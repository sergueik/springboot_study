package com.bookportal.api.exception;


import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.UNAUTHORIZED)
public class CustomUnauthorizedException extends RuntimeException {
    public CustomUnauthorizedException(String type) {
        super(type);
    }
}

