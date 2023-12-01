package com.example.demo.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */
// import org.junit.Before;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
// import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceTest {

	private int randomServerPort = 8085;

	private String route = null;
	private final static String body = "Greetings from Spring Boot!";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
	}

	@Disabled
	@Test
	public void test1() throws Exception {
		// Assumptions.assumeFalse(false);
		route = "/hello/";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.NOT_FOUND));
	}

	@Test
	public void test2() throws Exception {
		Assertions.assertThrows(HttpClientErrorException.class, () -> {
			route = "/hello/";
			url = "http://localhost:" + randomServerPort + route;
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.NOT_FOUND));

		});
	}

	@Test
	public void test3() throws Exception {
		route = "/hello";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat("Unexpected response for " + route, responseEntity.getBody(), is(body));
	}
}
