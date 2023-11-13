package example.controller;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import com.google.gson.Gson;
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

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

@SuppressWarnings("unchecked")
public class RemoteHosTest {

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
	private Map<String, String> data = new HashMap<>();

	@BeforeEach
	public void setUp() {

	}

	// correct charset
	@Test
	public void test1() throws Exception {
		route = "/basic/request";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + route,
				data.containsKey("remotehost"), is(true));
		assertThat("Unexpected remotehost", data.get("remotehost"),
				is("127.0.0.1"));
	}

	@Test
	public void test2() throws Exception {
		route = "/basic/context";
		url = "http://localhost:" + randomServerPort + route;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + route,
				data.containsKey("remotehost"), is(true));
		assertThat("Unexpected remotehost", data.get("remotehost"),
				is("127.0.0.1"));
	}

}
