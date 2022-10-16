package com.serverdemo.server.model;

public class KafkaMessage<T> {
    private T data;

    public KafkaMessage(T data) {
        this.data = data;
    }

    public T getData() {
        return data;
    }
}
