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
		"serverPort=8080" })
@PropertySource("classpath:application.properties")

public class AcceptanceTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	// NOTE: property annotations have no effect
	// @Value("${server.port:8085}")
	// NOTE: property annotations have no effect
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8080;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests

	// ResourceAccess I/O error on GET request:
	// org.springframework.web.client.ResourceAccessException:
	// I/O error on GET request for "http://localhost:0/basic":
	// connect: Address is invalid on local machine, or
	// port is not valid on remote machine

	private final String route = "/homepage";
	// NOTE: execrising property file override
	private final static String body = "Hello test data";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private final String url2 = "http://localhost:" + randomServerPort + route;
	// cannot cast
	// private final List<MediaType> mediaTypes = new
	// ArrayList<MediaType>(Arrays.asList(new MediaType[] {
	// MediaType.APPLICATION_JSON })));
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test1() throws Exception {
		Assumptions.assumeFalse(false);
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), is(body));
	}
}
