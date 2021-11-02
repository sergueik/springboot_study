package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */
// import org.junit.Before;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;


@PropertySource("classpath:application.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8081" })
public class ExampleAcceptanceTest {

	private final String route1 = "/api/v1/nonsecure";
	private final String route2 = "/api/v1/secure";
	private static final RestTemplate restTemplate = new RestTemplate();

	@Value("${header:API_KEY}")
	private String headerName;
	@Value("${secret}")
	private String headerValue;
	@LocalServerPort
	private int port = 8081;

	private final String host = "http://localhost:" + port;
	private String url = null;
	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<String> responseEntity = null;

	@Test
	public void test1() throws Exception {
		url = host + route1;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test2() throws Exception {
		Assertions.assertThrows(HttpClientErrorException.class, () -> {
			url = host + route2;
			responseEntity = restTemplate.getForEntity(url, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNAUTHORIZED));
		});
	}

	// NOTE: No RestTemplate get* methods offer accept the request as argument
	// https://stackoverflow.com/questions/19238715/how-to-set-an-accept-header-on-spring-resttemplate-request
	// https://stackoverflow.com/questions/21101250/sending-get-request-with-authentication-headers-using-resttemplate
	// https://www.baeldung.com/rest-template
	@Test
	public void test3() throws Exception {
		url = host + route2;
		headers = new HttpHeaders();
		headers.add(headerName, headerValue);

		HttpEntity<String> request = new HttpEntity<String>("", headers);
		ResponseEntity<String> responseEntity = restTemplate.exchange(url,
				HttpMethod.GET, request, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test4() throws Exception {
		Assertions.assertThrows(HttpClientErrorException.class, () -> {
			url = host + route2;
			headers = new HttpHeaders();
			headers.add(headerName, "wrong key");

			HttpEntity<String> request = new HttpEntity<String>("", headers);
			ResponseEntity<String> responseEntity = restTemplate.exchange(url,
					HttpMethod.GET, request, String.class);
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNAUTHORIZED));
		});
	}

}
