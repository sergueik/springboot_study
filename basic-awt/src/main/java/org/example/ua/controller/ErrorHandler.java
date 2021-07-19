package org.example.ua.controller;

import lombok.extern.slf4j.Slf4j;
import org.example.ua.dto.response.ErrorResponse;
import org.example.ua.exception.ResponseException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;

import javax.validation.ConstraintViolationException;
import java.util.Date;
import java.util.stream.Collectors;

@ControllerAdvice
@Slf4j
public class ErrorHandler {

    @ExceptionHandler(Throwable.class)
    public final ResponseEntity<ErrorResponse> handleResponseException(Throwable ex, WebRequest request) {
        ErrorResponse errorResponse = new ErrorResponse(
                new Date(),
                getMessage(ex),
                request.getDescription(false)
        );
        log.error(errorResponse.getMessage());
        return new ResponseEntity<>(errorResponse, getHttpStatus(ex));
    }

    private String getMessage(Throwable ex) {
        if (ex instanceof ConstraintViolationException) {
            return ((ConstraintViolationException) ex)
                    .getConstraintViolations().stream()
                    .map(cv -> cv.getPropertyPath().toString() + " " + cv.getMessage())
                    .collect(Collectors.joining(";"));
        } else if (ex instanceof MethodArgumentNotValidException) {
            return ((MethodArgumentNotValidException) ex)
                    .getBindingResult().getFieldErrors().stream()
                    .map(cv -> cv.getField() + " " + cv.getDefaultMessage())
                    .collect(Collectors.joining(";"));
        }
        return ex.getMessage();
    }

    private HttpStatus getHttpStatus(Throwable ex) {
        if (ex instanceof ResponseException) {
            return ((ResponseException) ex).getHttpStatus();
        } else if (ex instanceof ConstraintViolationException
                || ex instanceof MethodArgumentNotValidException) {
            return HttpStatus.BAD_REQUEST;
        }
        return HttpStatus.INTERNAL_SERVER_ERROR;
    }
}
