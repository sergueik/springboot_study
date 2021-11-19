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
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;

import org.apache.commons.codec.binary.Base64;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class ApplicationTests {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int randomServerPort = 8085;

	private final String route = "/employees/";
	// NOTE: execrising property file override
	private final static String body = "Hello test data";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private HttpHeaders headers = new HttpHeaders();
	private final static String username = "admin";
	private final static String password = "password";
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		url = "http://localhost:" + randomServerPort + route;
	}

	@Test
	public void test1() throws Exception {

		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNAUTHORIZED));
		} catch (HttpClientErrorException e) {
			System.err.println(e.getMessage());
		}
	}

	@Test
	public void test2() throws Exception {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		headers.setBasicAuth(username, password);
		System.err.println("Authorization:" + headers.get("Authorization"));
		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		} catch (HttpClientErrorException e) {
			System.err.println(e.getMessage());
		}
	}

	@Test
	public void test3() throws Exception {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		String encodedCredentials = null;
		try {
			encodedCredentials = new String(Base64.encodeBase64(
					String.format("%s:%s", username, password).getBytes("UTF8")));
		} catch (UnsupportedEncodingException e) {
			System.err.println("Exception (ignored): " + e.toString());
		}

		headers.setBasicAuth(encodedCredentials);

		System.err.println("Authorization:" + headers.get("Authorization"));
		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		} catch (HttpClientErrorException e) {
			System.err.println(e.getMessage());
		}
	}

	// https://www.baeldung.com/spring-resttemplate-post-json
	// if switching to https atempted:
	// java.lang.IllegalArgumentException: Invalid character found in method name
	// [0x160x030x03...].
	// HTTP method names must be tokens
}
