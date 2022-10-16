package com.clientdemo.client.model;

public class KafkaMessage<V> {
    private V data;
    public V getData() {
        return data;
    }
}
