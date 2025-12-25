package com.bookportal.api.model;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class RequestLog {
    private String path;
    private String requestId;
    private String uri;
    private String remote;
    private String parameters;

    @Override
    public String toString() {
        return "RequestLog {" +
                "path='" + path + '\'' +
                ", requestId='" + requestId + '\'' +
                ", uri='" + uri + '\'' +
                ", remote='" + remote + '\'' +
                ", parameters='" + parameters + '\'' +
                '}';
    }
}

