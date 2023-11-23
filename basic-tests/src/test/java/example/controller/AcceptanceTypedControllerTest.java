package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

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

import org.springframework.web.client.HttpClientErrorException.NotFound;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import example.controller.ExampleController.Data;

import static org.hamcrest.Matchers.is;
// import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceTypedControllerTest {

	@LocalServerPort
	private int randomServerPort = 8085;
	private final String route = "/typed";
	// NOTE: execrising property file override
	private static final RestTemplate restTemplate = new RestTemplate();
	private static ExampleController.Data data = new ExampleController.Data();
	private String url = null;
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<Data> request1 = null;
	private HttpEntity<String> request2 = null;
	private ResponseEntity<String> responseEntity = null;
	private static final String name = "white";

	private static Gson gson = new GsonBuilder().create();

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	@Test
	public void test1() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "/data";

		data.setName(name);
		request2 = new HttpEntity<String>(
				gson.toJson(data, Data.class).toString() + "bad text", headers);
		System.err.println("request body: " + request2.getBody());
		responseEntity = restTemplate.postForEntity(url, request2, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String responseBody = responseEntity.getBody();
		System.err.println("responseBody: " + responseBody);
		assertThat(responseBody, containsString(name));

	}

	@Disabled("uncaught org.springframework.web.client.HttpClientErrorException$BadRequest:")
	@Test
	public void test3() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "/data";

		data.setName(name);
		request2 = new HttpEntity<String>("bad text", headers);
		System.err.println("request body: " + request2.getBody());
		responseEntity = restTemplate.postForEntity(url, request2, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.BAD_REQUEST));
		String responseBody = responseEntity.getBody();
		System.err.println("responseBody: " + responseBody);
		assertThat(responseBody, containsString("\"path\":\"/typed/data\""));

	}

	// @Disabled
	// TODO: Expected: a string containing "white" but: was "{"name":null}"
	@Test
	public void test2() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "/map";

		data.setName(name);

		request1 = new HttpEntity<Data>(data, headers);
		responseEntity = restTemplate.postForEntity(url, request1, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String responseBody = responseEntity.getBody();
		assertThat(responseBody, containsString(name));

	}
}
