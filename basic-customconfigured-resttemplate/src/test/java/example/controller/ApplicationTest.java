package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */
// import org.junit.Before;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.http.HttpStatus;

import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import example.controller.config.HttpClientConfig;
import example.controller.config.RestTemplateConfig;

import static org.hamcrest.Matchers.is;
// import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.hamcrest.Matchers.greaterThan;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

// NOTE: uncommenting the annotation leads to
// Unable to start ServletWebServerApplicationContext due to missing
// ServletWebServerFactory bean.
// logging shows that the RestTemplateConfig is being loaded even without the
// @ContextConfiguration

// @ContextConfiguration(classes = { RestTemplateConfig.class,
// HttpClientConfig.class })
public class ApplicationTest {

	@LocalServerPort
	private int randomServerPort = 8085;
	private final String route = "/basic";
	private final static String body = "hello basic";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;

	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		url = "http://localhost:" + randomServerPort + route;

	}

	@Test
	public void test1() throws Exception {
		Assumptions.assumeFalse(false);
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), is(body));
	}

}
