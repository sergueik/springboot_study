package example.acceptance;
/**
 * Copyright 2022,2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.client.HttpServerErrorException.InternalServerError;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceTest {

	@LocalServerPort
	private int randomServerPort = 8085;

	private String route = null;
	private String body = null;
	private String url = null;
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test1() throws Exception {
		// to skip dynamically use Junit 5 Assumptions
		// Assumptions.assumeFalse(false);
		route = "/jsp" + "?name=test1";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = "Hello test1";
		assertThat("Unexpected response for " + route, responseEntity.getBody(),
				containsString(body));
	}

	@Test
	public void test2() throws Exception {
		// to skip dynamically use Junit 5 Assumptions
		// Assumptions.assumeFalse(false);
		route = "/thymeleaf" + "?name=test2";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = "Hello <span>test2</span>";
		assertThat("Unexpected response for " + route, responseEntity.getBody(),
				containsString(body));
	}

	@Test
	public void test3() throws Exception {
		Assertions.assertThrows(InternalServerError.class, () -> {
			route = "/thymeleaf/misconfigured";
			url = "http://localhost:" + randomServerPort + route;
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(),
					is(HttpStatus.INTERNAL_SERVER_ERROR));

		});
	}

}
