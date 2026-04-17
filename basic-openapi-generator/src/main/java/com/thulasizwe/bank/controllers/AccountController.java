package com.thulasizwe.bank.controllers;

import com.thulasizwe.bank.api.AccountApi;
import com.thulasizwe.bank.dto.AccountResponse;
import com.thulasizwe.bank.dto.CreateAccountRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@Slf4j
public class AccountController implements AccountApi {
    @Override
    public ResponseEntity<AccountResponse> createAccount(CreateAccountRequest createAccountRequest) {
        return null;
    }

    @Override
    public ResponseEntity<AccountResponse> getAccount(UUID accountId) {
        return null;
    }
}
