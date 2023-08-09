package com.pocisoft.eventsapi.data;

public class APIResponse<T> {
    private int statusCode;
    private boolean isSuccess;
    private String errorMessage;
    private T data;

    public APIResponse(int statusCode, boolean isSuccess, String errorMessage, T data) {
        this.statusCode = statusCode;
        this.isSuccess = isSuccess;
        this.errorMessage = errorMessage;
        this.data = data;
    }

    public static <T> APIResponse<T> success(T data) {
        return new APIResponse<T>(200, true, null, data);
    }

    public static <T> APIResponse<T> failure(int statusCode, String errorMessage) {
        return new APIResponse<T>(statusCode, false, errorMessage, null);
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public boolean isSuccess() {
        return isSuccess;
    }

    public void setSuccess(boolean success) {
        isSuccess = success;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }
}
