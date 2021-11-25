package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

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

// NOTE: property annotations have no effect for serverPort

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestConfiguration
@SuppressWarnings("deprecation")
public class BasicAuthTests {

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
	}

	// see also: https://habr.com/ru/post/591305/ (in Russian)
	//
	@Test
	void test1() {

		HttpClientErrorException exception = Assertions
				.assertThrows(HttpClientErrorException.class, () -> {
					responseEntity = restTemplate.getForEntity(url, String.class);
					// the following line is not reached
					assertThat(responseEntity.getStatusCode(),
							is(HttpStatus.UNAUTHORIZED));
				});
		assertThat(exception.getMessage(), containsString(
				"Full authentication is required to access this resource"));
	}

	// based on discussion from:
	// https://stackoverflow.com/questions/39651097/how-to-add-basic-auth-to-autowired-testresttemplate-in-springboottest-spring-bo
	@Test
	public void test2() throws Exception {
		restTemplate.getInterceptors()
				.add(new BasicAuthorizationInterceptor(username, password));
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString(body));

	}

	// if switching to https atempted:
	// java.lang.IllegalArgumentException: Invalid character found in method name
	// [0x160x030x03...].
	// HTTP method names must be tokens
}
