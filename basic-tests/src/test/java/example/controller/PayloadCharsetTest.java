package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
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

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class PayloadCharsetTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	// NOTE: property annotations have no effect
	// @Value("${server.port:8085}")
	// NOTE: property annotations have no effect
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8085;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests
	private static String route = null;

	private static final RestTemplate restTemplate = new RestTemplate();
	private static ExampleController.Data data = new ExampleController.Data();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	// correct charset
	@Test
	public void test1() throws Exception {
		route = "/basic/charset";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is((true)));
		assertThat("Unexpected response headers for " + route,
				responseEntity.getHeaders().get("Content-Type").get(0),
				is(("text/plain;charset=UTF-8")));
		assertThat("Unexpected response for " + route, responseEntity.getBody(),
				is("тест"));
	}

	// incorrect charset encoding
	@Test
	public void test2() throws Exception {
		route = "/basic/charset2";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is((true)));
		assertThat("Unexpected response headers for " + route,
				responseEntity.getHeaders().get("Content-Type").get(0),
				is(("text/plain;charset=us-ascii")));
		assertThat("Unexpected response for " + route, responseEntity.getBody(),
				is("????"));
	}

}
