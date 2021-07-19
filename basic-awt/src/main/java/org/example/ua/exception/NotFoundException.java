package org.example.ua.exception;

import org.springframework.http.HttpStatus;

public class NotFoundException extends ResponseException {

    public NotFoundException(String message) {
        super(message, HttpStatus.NOT_FOUND);
    }
}
