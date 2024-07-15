package example.controller;

/**
 * Copyright 2024 Serguei Kouzmine
 */

// import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8085" })
@PropertySource("classpath:application.properties")

public class HTTPStatusControllerTest {
	@LocalServerPort
	private int randomServerPort = 8085;

	private final String route1 = "/basic/exception1";
	private final String route2 = "/basic/exception2";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test1() throws Exception {
		HttpStatus status = HttpStatus.BAD_GATEWAY;

		url = "http://localhost:" + randomServerPort + route1 + "/xxx";
		Exception exception = assertThrows(HttpServerErrorException.class, () -> {
			responseEntity = restTemplate.getForEntity(url, String.class);
		});
		assertThat(exception.getMessage(), containsString(Integer.toString(status.value())));
	}

	@Test
	public void test2() throws Exception {
		HttpStatus status = HttpStatus.BAD_GATEWAY;

		url = "http://localhost:" + randomServerPort + route1 + "/xxx";
		Exception exception = assertThrows(RestClientException.class, () -> {
			responseEntity = restTemplate.getForEntity(url, String.class);
		});
		// for calls leading to exceptions
		assertThat(exception.getMessage(), containsString(Integer.toString(status.value())));
		// for calls not leading to exceptions
		// assertThat("Expected code: " + statusValue,
		// responseEntity.getStatusCode(), is(status));
	}

	@Test
	public void test3() {

		HttpStatus status = HttpStatus.FORBIDDEN;
		url = "http://localhost:" + randomServerPort + route2;

		Exception exception = assertThrows(RestClientException.class, () -> {
			responseEntity = restTemplate.getForEntity(url, String.class);
		});
		assertThat(exception.getMessage(), containsString(Integer.toString(status.value())));

	}

}
