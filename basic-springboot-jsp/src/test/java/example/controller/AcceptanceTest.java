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
import org.springframework.http.HttpStatus;

import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class AcceptanceTest {

	@LocalServerPort
	// NOTE: property annotations have no effect
	// @Value("${server.port:8085}")
	// NOTE: property annotations have no effect
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8085;

	// @LocalServerPort
	// @Autowired
	// @LocalServerPort
	// @Value("${server.port:8085}")
	// @Value("${serverPort}")
	// private int randomServerPort;
	// error:
	// org.springframework.web.client.ResourceAccessException: I/O error on GET
	// request
	// for "http://localhost:8085/": Connection refused: connect; nested exception
	// is
	// java.net.ConnectException: Connection refused: connect

	private final String route = "/";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = "http://localhost:" + randomServerPort + route;
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	@Disabled
	@Test
	public void test1() throws Exception {
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(), containsString("Hello World"));
	}

	@Disabled
	@Test
	public void test2() throws Exception {
		String name = "value";
		responseEntity = restTemplate.getForEntity(url + "?name=" + name,
				String.class);
		assertThat(responseEntity.getBody(),
				containsString(String.format("Hello %s", name)));
	}

	@Test
	public void test3() throws Exception {
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

}
