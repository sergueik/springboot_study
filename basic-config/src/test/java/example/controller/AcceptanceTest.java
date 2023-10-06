package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.util.Arrays;
import java.util.Map;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
//import org.junit.Before;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.utils.Utils;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceTest {

	private static Gson gson = new GsonBuilder().create();

	@LocalServerPort
	private int randomServerPort = 8085;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests

	private static String route = null;
	private static String body = null;
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;

	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	@Test
	public void test1() throws Exception {
		Assumptions.assumeFalse(false);
		route = "/configs/file_hash_status?filename=a.txt&newer=12345";
		Map<String, Object> response = Utils.getErrorResponse("newer: 12345");
		body = gson.toJson(response, Map.class); 
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.ALREADY_REPORTED));
		assertThat("Unexpected response body for " + route,
				responseEntity.getBody(), is(body));
	}

	@Test
	public void test2() throws Exception {
		Assumptions.assumeFalse(false);
		route = "/configs/file_hash_status?filename=a.txt&hash=x";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.NOT_MODIFIED));
		assertThat("Unexpected response body for " + route,
				responseEntity.getBody(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void test3() throws Exception {
		route = "/configs/file_hash?filename=a.txt&hash=x";
		url = "http://localhost:" + randomServerPort + route;

		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String responseBody = responseEntity.getBody();
		Map<String, Object> response = gson.fromJson(responseBody, Map.class);
		assertThat(response, notNullValue());
		for (String key : Arrays.asList(new String[] { "result", "status" })) {
			assertThat(response.containsKey(key), is(true));
		}
		assertThat(response.get("result"), is("hash"));

	}

	@Test
	@SuppressWarnings("unchecked")
	public void test4() throws Exception {
		route = "/configs/file_hash?filename=a.txt&newer=12345";
		url = "http://localhost:" + randomServerPort + route;

		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String responseBody = responseEntity.getBody();
		Map<String, Object> response = gson.fromJson(responseBody, Map.class);
		assertThat(response, notNullValue());
		for (String key : Arrays.asList(new String[] { "result", "status" })) {
			assertThat(response.containsKey(key), is(true));
		}
		assertThat(response.get("result"), is("error message"));

	}
}
