package com.thulasizwe.bank.http;

import com.thulasizwe.bank.api.PaymentClientApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.support.RestClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;

@Configuration
public class ClientConfig {
    @Bean
    public PaymentClientApi paymentClient() {

        RestClient transferRestClient = RestClient.builder()
                .baseUrl("http://localhost:8080/v1/api/payments")
                .build();

        HttpServiceProxyFactory factory = HttpServiceProxyFactory
                .builderFor(RestClientAdapter.create(transferRestClient))
                .build();

        return factory.createClient(PaymentClientApi.class);
    }
}
