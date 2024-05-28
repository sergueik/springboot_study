package example.controller;

/**
 * Copyright 2024 Serguei Kouzmine
 */

// import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.client.RestClientException;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8085" })
@PropertySource("classpath:application.properties")

// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/http/HttpStatus.html
public class StatusCodeControllerTest {
	@LocalServerPort
	private int randomServerPort = 8085;
	// NOTE: when randomServerPort is not initialized sets set to zero
	// which leads to crashing tests

	private final String route = "/basic/statuscode";
	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {

	}

	@Disabled("need to find a different way to confirm HTTP Status 400 Bad Request")
	@Test
	public void test1() throws Exception {
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.BAD_REQUEST));
	}

	@Disabled("need to find a different way to confirm HTTP Status 404 Not Found")
	@Test
	public void test2() throws Exception {
		url = "http://localhost:" + randomServerPort + route + "?code=404";
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.NOT_FOUND));
	}

	@Test
	public void test3() throws Exception {
		for (HttpStatus status : Arrays.asList(HttpStatus.NOT_MODIFIED, HttpStatus.ALREADY_REPORTED)) {
			int statusValue = status.value();
			url = "http://localhost:" + randomServerPort + route + "?code=" + statusValue;
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat("Expected code: " + statusValue, responseEntity.getStatusCode(), is(status));
		}
	}

	@Test
	public void test4() {

		for (HttpStatus status : Arrays.asList(HttpStatus.NOT_IMPLEMENTED, HttpStatus.SERVICE_UNAVAILABLE,
				HttpStatus.UNAUTHORIZED, HttpStatus.TOO_MANY_REQUESTS, HttpStatus.NOT_FOUND, HttpStatus.GONE)) {

			int statusValue = status.value();
			url = "http://localhost:" + randomServerPort + route + "?code=" + statusValue;
			try {
				Thread.sleep(5000);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}

			Exception exception = assertThrows(Exception.class, () -> {
				responseEntity = restTemplate.getForEntity(url, String.class);
			});
			assertThat(exception.getMessage(), containsString("" + statusValue));
		}
	}

	@Test
	public void test5() {
		for (int status : Arrays.asList(501, 503, 403, 429, 404, 410)) {
			url = "http://localhost:" + randomServerPort + route + "?code=" + status;
			Exception exception = assertThrows(Exception.class, () -> {
				responseEntity = restTemplate.getForEntity(url, String.class);
			});
			assertThat(exception.getMessage(), containsString("" + status));
		}
	}

	@Test
	public void test6() {
		url = "http://localhost:" + randomServerPort + route;
		Exception exception = assertThrows(Exception.class, () -> {
			responseEntity = restTemplate.getForEntity(url, String.class);
		});
		assertThat(exception.getMessage(), containsString("400"));
	}

	@Test
	public void test7() {
		int timeout = 5000;
		Exception exception = assertThrows(Exception.class, () -> {
			int retry = 5;
			int statusValue = HttpStatus.SERVICE_UNAVAILABLE.value();
			url = "http://localhost:" + randomServerPort + route + "?code=" + statusValue;
			while (retry > 0) {
				try {
					responseEntity = restTemplate.getForEntity(url, String.class);
				} catch (RestClientException e1) {

					System.err.println("Processing exception: " + e1.getMessage());
					if (e1.getMessage().indexOf("" + statusValue) != -1) {
						try {
							Thread.sleep(timeout);
						} catch (InterruptedException e2) {
							e2.printStackTrace();
						}
						// decrement retry counter
						retry--;
					} else {
						throw new RuntimeException(
								String.format("Unexpected exception %s accessing %s", e1.toString(), url));
					}
				}
			}

			if (retry <= 0) {
				throw new RuntimeException(String.format("run out of retries accessing %s", url));
			} else {
				// handle responseEntity
			}
		});
		assertThat(exception.getMessage(), containsString("run out of retries"));
	}

}
