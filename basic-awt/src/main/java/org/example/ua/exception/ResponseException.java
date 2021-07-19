package org.example.ua.exception;

import org.springframework.http.HttpStatus;

public class ResponseException extends RuntimeException {

    private final HttpStatus httpStatus;

    public ResponseException(String message, HttpStatus httpStatus) {
        super(message);
        this.httpStatus = httpStatus;
    }

    public HttpStatus getHttpStatus() {
        return httpStatus;
    }
}
