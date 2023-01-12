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

public class Uu03dFixTest {

	@LocalServerPort
	private int randomServerPort = 8085;

	private String url = null;
	private static String route = "/basic/uu03d";
	private static final Gson gson = new Gson();
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
	}

	private static String body;

	// @Disabled
	@Test
	public void test1() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "?fix=false";
		responseEntity = restTemplate.getForEntity(url, String.class);
		// NOTE: different test from legacy Spring
		body = "0\\u003dRUNNING,1\\u003dRUNNING,2\\u003dRUNNING";
		System.err.println(
				"get: " + gson.fromJson(responseEntity.getBody(), Object.class));
		// get: {data=0=RUNNING,1=RUNNING,2=RUNNING}
		// being converted by tool ?
		body = "0\u003dRUNNING,1\u003dRUNNING,2\u003dRUNNING";
		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test2() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "?fix=true";
		responseEntity = restTemplate.getForEntity(url, String.class);
		body = "0=RUNNING,1=RUNNING,2=RUNNING";

		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test3() throws Exception {
		url = "http://localhost:" + randomServerPort + "/basic/uu03draw";
		responseEntity = restTemplate.getForEntity(url, String.class);
		body = "0=RUNNING,1=RUNNING,2=RUNNING";
		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}

	@Test
	public void test5() throws Exception {
		url = "http://localhost:" + randomServerPort + "/basic/uu03draw";
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is(true));
		assertThat(responseEntity.getHeaders().get("Content-Type").get(0),
				containsString("application/json"));
	}

	@Test
	public void test6() throws Exception {
		url = "http://localhost:" + randomServerPort + "/basic/uu03dcharset";
		responseEntity = restTemplate.getForEntity(url, String.class);
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getHeaders().containsKey("Content-Type"),
				is(true));
		assertThat(responseEntity.getHeaders().get("Content-Type").get(0),
				containsString("application/json"));
		body = "0\\u003dRUNNING,1\\u003dRUNNING,2\\u003dRUNNING";
		assertThat(responseEntity.getBody(), containsString(body));

		body = "0=RUNNING,1=RUNNING,2=RUNNING";
		assertThat("Unexpected formatting for " + route,
				gson.fromJson(responseEntity.getBody(), Object.class).toString(),
				containsString(body));
	}
}
