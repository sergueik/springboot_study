package com.bookportal.api.response;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Builder;
import lombok.Data;
import org.springframework.http.HttpStatus;

import java.sql.Timestamp;


@Data
@Builder
@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class CustomResponse {
    private Timestamp timestamp;
    private int status;
    private Object error;
    private Object message;
    private Object path;
    private Object data;

    public static CustomResponse responseOk(Object data) {
        return CustomResponse.builder()
                .timestamp(new Timestamp(System.currentTimeMillis()))
                .status(HttpStatus.OK.value())
                .message("Success")
                .data(data).build();
    }

    public static CustomResponse responseCreated(Object data) {
        return CustomResponse.builder()
                .timestamp(new Timestamp(System.currentTimeMillis()))
                .status(HttpStatus.CREATED.value())
                .message("Success")
                .data(data).build();
    }

    public static CustomResponse responseError(Object data, Object err, Object path, int status) {
        return CustomResponse.builder()
                .timestamp(new Timestamp(System.currentTimeMillis()))
                .status(status)
                .path(path)
                .error(err)
                .message(data)
                .build();
    }
}
