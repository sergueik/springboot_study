package com.serverdemo.server.model;

import java.util.UUID;

public class RequestDto {
    private UUID requestId;
    private String data;

    public RequestDto(UUID requestId, String data) {
        this.requestId = requestId;
        this.data = data;
    }

    public UUID getRequestId() {
        return requestId;
    }

    public String getData() {
        return data;
    }
}
