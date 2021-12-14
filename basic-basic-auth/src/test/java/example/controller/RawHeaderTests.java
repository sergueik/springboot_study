package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
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

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestConfiguration
public class RawHeaderTests {

	@LocalServerPort
	private int randomServerPort = 8086;

	private final String route = "/employees/";
	private String url = null;
	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<String> responseEntity = null;
	private final static String body = "McDonald";

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;

	private static final RestTemplate restTemplate = new RestTemplate();
	// .withBasicAuth method is not available for specific release of template
	// private static final RestTemplate restTemplate = new
	// RestTemplate().withBasicAuth(username, password);

	@BeforeEach
	public void setUp() {
		url = "http://localhost:" + randomServerPort + route;
		headers = new HttpHeaders();
	}

	@AfterEach
	public void after() {
		System.err.println("Authorization:" + headers.get("Authorization"));
		// echo 'YWRtaW46cGFzc3dvcmQ=' | base64 -d -
		// admin:password
	}

	// NOTE: building headers by hand has no effect
	@Test
	public void test1() throws Exception {
		headers.setBasicAuth(username, password);
		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		} catch (HttpClientErrorException e) {
			System.err.println(e.getMessage());
		}
	}

	@Test
	public void test2() throws Exception {
		String encodedCredentials = null;
		try {
			encodedCredentials = new String(Base64.encodeBase64(
					String.format("%s:%s", username, password).getBytes("UTF8")));
		} catch (UnsupportedEncodingException e) {
			System.err.println("Exception (ignored): " + e.toString());
		}

		// building headers by hand has no effect
		headers.setBasicAuth(encodedCredentials);

		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		} catch (HttpClientErrorException e) {
			System.err.println(e.getMessage());
		}
	}
}
