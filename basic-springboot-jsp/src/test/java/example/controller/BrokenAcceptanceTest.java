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

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class BrokenAcceptanceTest {

	@LocalServerPort
	private int serverPort = 8085;

	// NOTE: property annotations have no effect
	// @LocalServerPort
	// @Autowired
	// @LocalServerPort
	// @Value("${server.port:8085}")
	// @Value("${serverPort}")
	// private int serverPort;

	// NOTE: when not @Autowired and not initialized sets to zero,
	// which leads to crashing tests
	// ResourceAccess I/O error on GET request:
	// org.springframework.web.client.ResourceAccessException:
	// I/O error on GET request for "http://localhost:0/basic":
	// connect: Address is invalid on local machine, or
	// port is not valid on remote machine

	private final String route = "/model";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = "http://localhost:" + serverPort + route;
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private final boolean badtest = Boolean
			.parseBoolean(System.getenv("BADTEST"));
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		if (!badtest) {
			url = "http://localhost:" + serverPort + route;
		}
		// when BADTEST is set, test fail with
		// error: org.springframework.web.client.ResourceAccessException:
		// I/O error on GET request for "http://localhost:8085/model":
		// Connection refused: connect;
	}

	@Test
	public void test1() throws Exception {
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(), containsString("Hello World"));
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

}
