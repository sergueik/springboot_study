package com.clientdemo.client.service;

import java.util.concurrent.TimeoutException;

public interface SyncKafkaService {
    String get(String text) throws TimeoutException;
}
