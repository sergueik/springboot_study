package com.bookportal.api.exception;

import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.CategoryService;

import lombok.extern.slf4j.Slf4j;
//the lombok.extern.slf4j.Sld4j does not work.
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.bind.support.WebExchangeBindException;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.stream.Collectors;

@Slf4j
@ControllerAdvice
@RestControllerAdvice
public class ExceptionResolver {
	private final Logger log = LoggerFactory.getLogger(ExceptionResolver.class);

    @ExceptionHandler(WebExchangeBindException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public Mono<CustomResponse> handleWebExchangeBindException(WebExchangeBindException e, ServerHttpRequest request) {
        log.error("Exception caught in handleWebExchangeBindException: " + e.getMessage());
        return Mono.fromCallable(() -> e.getBindingResult()
                        .getAllErrors()
                        .stream()
                        .map(DefaultMessageSourceResolvable::getDefaultMessage)
                        .collect(Collectors.toList()))
                .map(messages -> CustomResponse.responseError
                        (messages, e.getReason(), request.getPath().value(),HttpStatus.BAD_REQUEST.value()));
    }

    @ExceptionHandler(ResponseStatusException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public Mono<CustomResponse> handleResponseStatusException(ResponseStatusException e, ServerHttpRequest request) {
        log.error("Exception caught in handleResponseStatusException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), e.getReason(), request.getPath().value(),HttpStatus.BAD_REQUEST.value()));
    }

    @ExceptionHandler(AccessDeniedException.class)
    @ResponseStatus(HttpStatus.FORBIDDEN)
    public Mono<CustomResponse> handleAccessDeniedException(AccessDeniedException e, ServerHttpRequest request) {
        log.error("Exception caught in handleAccessDeniedException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), "Access Denied", request.getPath().value(),HttpStatus.FORBIDDEN.value()));
    }

    @ExceptionHandler(CustomAlreadyExistException.class)
    @ResponseStatus(HttpStatus.CONFLICT)
    public Mono<CustomResponse> handleCustomAlreadyExistException(CustomAlreadyExistException e, ServerHttpRequest request) {
        log.error("Exception caught in handleCustomAlreadyExistException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), "Already Exists", request.getPath().value(),HttpStatus.CONFLICT.value()));
    }

    @ExceptionHandler(CustomNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    public Mono<CustomResponse> handleCustomNotFoundException(CustomNotFoundException e, ServerHttpRequest request) {
        log.error("Exception caught in handleCustomNotFoundException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), "Not Found", request.getPath().value(),HttpStatus.NOT_FOUND.value()));
    }

    @ExceptionHandler(CustomUnauthorizedException.class)
    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    public Mono<CustomResponse> handleCustomUnauthorizedException(CustomUnauthorizedException e, ServerHttpRequest request) {
        log.error("Exception caught in handleCustomUnauthorizedException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), "Unauthorized", request.getPath().value(),HttpStatus.UNAUTHORIZED.value()));
    }

    @ExceptionHandler(CustomFileUploadException.class)
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    public Mono<CustomResponse> handleCustomFileUploadException(CustomFileUploadException e, ServerHttpRequest request) {
        log.error("Exception caught in handleCustomFileUploadException: " + e.getMessage());
        return Mono.fromCallable(() -> CustomResponse.responseError
                (e.getMessage(), "File Upload Error", request.getPath().value(),HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }
}
