package com.clientdemo.client.service;

import com.clientdemo.client.model.RequestDto;
import com.clientdemo.client.senderreceiver.SenderReceiverMap;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;
import java.util.concurrent.TimeoutException;

@Service
public class SyncKafkaServiceImp implements SyncKafkaService{

    @Autowired
    private SenderReceiverMap<UUID, String> senderReceiverMap;

    @Value("${endpointServer}")
    private String endpointServer;

    @Value("${timeout:0}")
    private Long timeout;

    public String get(String text) throws TimeoutException {
        UUID requestId = UUID.randomUUID();
        while (senderReceiverMap.containsKey(requestId)) {
            requestId = UUID.randomUUID();
        }

        String responseFromServer = this.sendText(requestId, text);
        System.out.println("REST response from server: " + responseFromServer);

        Thread thread = senderReceiverMap.add(requestId, timeout);
        thread.start();
        try {
            thread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        String responseKafka;
        try {
            responseKafka = senderReceiverMap.get(requestId).getData();
        } catch (TimeoutException e) {
            throw e;
        } finally {
            senderReceiverMap.remove(requestId);
        }
        return responseKafka;
    }

    private String sendText(final UUID requestId, final String text) {
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        String requestJsonStr = null;
        try {
            requestJsonStr = new ObjectMapper().writeValueAsString(new RequestDto(requestId, text));
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        HttpEntity<String> request = new HttpEntity<>(requestJsonStr, headers);
        return restTemplate.postForObject(endpointServer, request, String.class);
    }
}
