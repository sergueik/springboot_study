package com.serverdemo.server.kafka;

import com.serverdemo.server.model.KafkaMessage;

import java.util.UUID;

public interface KafkaMessageSender {
    void send(UUID requestId, KafkaMessage<String> message);
}
