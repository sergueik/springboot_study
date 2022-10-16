package com.clientdemo.client.controller;

import com.clientdemo.client.service.SyncKafkaService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.concurrent.TimeoutException;


@RestController
@RequestMapping("")
public class ClientController {

    private final SyncKafkaService syncKafkaService;

    public ClientController(SyncKafkaService syncKafkaService) {
        this.syncKafkaService = syncKafkaService;
    }

    @GetMapping("/test")
    public ResponseEntity<String> test(String text) {
        String result = null;
        try {
            result = syncKafkaService.get(text);
        } catch (TimeoutException e) {
            return new ResponseEntity<>(HttpStatus.GATEWAY_TIMEOUT);
        }
        return ResponseEntity.ok(result);
    }

}
