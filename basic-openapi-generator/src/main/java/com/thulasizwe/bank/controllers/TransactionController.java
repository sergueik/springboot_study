package com.thulasizwe.bank.controllers;

import com.thulasizwe.bank.api.TransactionApi;
import com.thulasizwe.bank.dto.CreateTransactionRequest;
import com.thulasizwe.bank.dto.TransactionResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@Slf4j
public class TransactionController implements TransactionApi {
    @Override
    public ResponseEntity<TransactionResponse> createTransaction(CreateTransactionRequest createTransactionRequest) {
        return null;
    }
}
