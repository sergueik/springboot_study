package com.bookportal.api.exception;


import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.CONFLICT)
public class CustomAlreadyExistException extends RuntimeException {
    public CustomAlreadyExistException(String type) {
        super(type);
    }
}

