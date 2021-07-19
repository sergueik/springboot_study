package org.example.ua.exception;

import org.springframework.http.HttpStatus;

public class ValidationResponseException extends ResponseException {

    public ValidationResponseException(String message) {
        super(message, HttpStatus.BAD_REQUEST);
    }
}
