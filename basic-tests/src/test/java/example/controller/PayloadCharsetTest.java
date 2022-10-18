package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class PayloadCharsetTest {

	@LocalServerPort
	// NOTE: property annotations seem to have no effect here
	// @Value("${server.port:8085}")
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
		route = "/basic/relevant_charset";
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
		route = "/basic/wrong_charset";
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
