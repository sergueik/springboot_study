package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
// import static org.hamcrest.Matchers.containsString;

// NOTE: Compilation failure
// reference to containsString is ambiguous
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import com.google.gson.Gson;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class HtmlEscapeControllerTest {

	@LocalServerPort
	private int randomServerPort = 8085;

	private String url = null;
	private static String route;
	private static final Gson gson = new Gson();
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;
	private final String body = "0=RUNNING,1=RUNNING,2=RUNNING";
	private final String body_escaped = body.replaceAll("=", "\\\\u003d");

	@BeforeEach
	public void setUp() {
	}

	// @Disabled
	@Test
	public void test1() throws Exception {

		route = "/htmlescape/basic";
		url = "http://localhost:" + randomServerPort + route + "?fix=false";
		responseEntity = restTemplate.getForEntity(url, String.class);
		// NOTE: different test from legacy Spring
		System.err.println(
				"get: " + gson.fromJson(responseEntity.getBody(), Object.class));
		assertThat("Unexpected formatting for " + route, responseEntity.getBody(),
				containsString(body_escaped));
		// observed auto conversion
		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test2() throws Exception {
		route = "/htmlescape/basic";
		url = "http://localhost:" + randomServerPort + route + "?fix=true";
		responseEntity = restTemplate.getForEntity(url, String.class);

		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test3() throws Exception {
		route = "/htmlescape/legacy";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test5() throws Exception {
		route = "/htmlescape/legacy";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is(true));
		assertThat(responseEntity.getHeaders().get("Content-Type").get(0),
				containsString("application/json"));
	}

	@Test
	public void test6() throws Exception {
		route = "/htmlescape/with_charset";
		url = "http://localhost:" + randomServerPort + route;

		responseEntity = restTemplate.getForEntity(url, String.class);
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is(true));
		assertThat(responseEntity.getHeaders().get("Content-Type").get(0),
				containsString("application/json"));
		assertThat(responseEntity.getBody(), containsString(body_escaped));

		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}
}
