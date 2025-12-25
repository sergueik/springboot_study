package com.bookportal.api.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.NOT_FOUND)
public class MyUserNotFoundException extends RuntimeException {
    public MyUserNotFoundException(String message) {
        super(message);
    }
}
