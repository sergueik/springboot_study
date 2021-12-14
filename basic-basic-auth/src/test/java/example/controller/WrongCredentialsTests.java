package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

// NOTE: placing the test method with bad credentials in the sameclass as with valid makes the latter test fail

@SuppressWarnings("deprecation")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestConfiguration
public class WrongCredentialsTests {

	@LocalServerPort
	private int randomServerPort = 8086;

	private final String route = "/employees/";
	private String url = null;
	// NOTE: initializing the url right here leads to
	// org.springframework.web.client.ResourceAccessException
	// GET request for "http://localhost:8086/employees/": Connection refused:
	// connect;
	// nested exception is java.net.ConnectException: Connection refused: connect
	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;
	private String password = "wrong password";

	private static final RestTemplate restTemplate = new RestTemplate();

	@Test
	public void test() throws Exception {
		url = "http://localhost:" + randomServerPort + route;
		HttpClientErrorException exception = Assertions
				.assertThrows(HttpClientErrorException.class, () -> {
					restTemplate.getInterceptors()
							.add(new BasicAuthorizationInterceptor(username, password));
					responseEntity = restTemplate.getForEntity(url, String.class);
					// the following line is not reached
					assertThat(responseEntity.getStatusCode(),
							is(HttpStatus.UNAUTHORIZED));
				});
		assertThat(exception.getMessage(), containsString("Bad credentials"));
	}

}
