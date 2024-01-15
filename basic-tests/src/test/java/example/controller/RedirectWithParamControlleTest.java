package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import com.google.gson.Gson;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

@SuppressWarnings("unchecked")
public class RedirectWithParamControlleTest {

	@LocalServerPort
	// NOTE: property annotations seem to have no effect here
	// @Value("${server.port:8085}")
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8085;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests
	private static String route = null;

	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;
	private final Gson gson = new Gson();
	private String body;
	private final String value = "42";
	private Map<String, String> data = new HashMap<>();

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test1() throws Exception {
		route = "/basic/redirect1" + "?param=" + value;
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + route, data.containsKey("param"),
				is(true));
		assertThat("Unexpected param value: " + data.get("param"),
				data.get("param"), is(value));
	}

	@Test
	public void test2() throws Exception {
		route = "/basic/redirect2" + "?param=" + value;
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + route, data.containsKey("param"),
				is(true));
		assertThat("Unexpected param value: " + data.get("param"),
				data.get("param"), is(value));
	}

}
