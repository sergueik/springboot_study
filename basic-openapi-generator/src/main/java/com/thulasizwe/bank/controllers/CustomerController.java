package com.thulasizwe.bank.controllers;

import com.thulasizwe.bank.api.CustomerApi;
import com.thulasizwe.bank.dto.CreateCustomerRequest;
import com.thulasizwe.bank.dto.CustomerListResponse;
import com.thulasizwe.bank.dto.CustomerResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequiredArgsConstructor
@Slf4j
public class CustomerController implements CustomerApi {

    @Override
    public ResponseEntity<CustomerResponse> createCustomer(CreateCustomerRequest createCustomerRequest) {
        return null;
    }

    @Override
    public ResponseEntity<CustomerResponse> getCustomer(UUID customerId) {
        return null;
    }

    @Override
    public ResponseEntity<CustomerListResponse> listCustomers(Integer page, Integer size) {
        return null;
    }
}