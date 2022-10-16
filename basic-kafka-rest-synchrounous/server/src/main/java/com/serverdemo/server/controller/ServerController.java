package com.serverdemo.server.controller;

import com.serverdemo.server.kafka.KafkaMessageSender;
import com.serverdemo.server.model.KafkaMessage;
import com.serverdemo.server.model.RequestDto;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.ThreadLocalRandom;

@RestController
@RequestMapping("")
public class ServerController {

    private final KafkaMessageSender kafkaMessageSender;

    public ServerController(KafkaMessageSender kafkaMessageSender) {
        this.kafkaMessageSender = kafkaMessageSender;
    }

    @PostMapping("/test")
    public String test(@RequestBody RequestDto request) {

        Runnable runnable =
                () -> {
                    System.out.println("Start requestId: " + request.getRequestId() + "   text: " + request.getData());

                    try {
                        int sleepMs = ThreadLocalRandom.current().nextInt(0, 17000 + 1);
                        System.out.println("RequestId: " + request.getRequestId() + " sleep: " + sleepMs + "ms");
                        Thread.sleep(sleepMs);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }

                    kafkaMessageSender.send(request.getRequestId(), new KafkaMessage<>(request.getData().toUpperCase()));

                    System.out.println("End requestId: " + request.getRequestId());
                };
        Thread thread = new Thread(runnable);
        thread.start();

        return "Ok!";
    }
}
