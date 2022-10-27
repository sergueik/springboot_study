package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
// Compilation failure
// reference to containsString is ambiguous
// import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceJSONPayloadTest {

	@LocalServerPort
	private int randomServerPort = 8085;

	private String url = null;
	private final String route = "/basic/json";
	private static final Gson gson = new Gson();
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
	}

	// https://www.tabnine.com/code/java/methods/org.springframework.http.HttpHeaders/containsKey
	// @Disabled
	@Test
	public void test1() throws Exception {
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is(true));
		assertThat(responseEntity.getHeaders().get("Content-Type").get(0),
				containsString("application/json"));
		assertThat("Unexpected response for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class), notNullValue());
	}
}
