package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import org.springframework.web.client.RestTemplate;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class AcceptanceTest {

	// NOTE: BeanPostProcessor :
	// Autowired annotation is not supported on static fields:
	@LocalServerPort
	private int serverPort = 8085;

	private final String route = "/model";
	private String base_url = null;
	// NOTE: exercising property file override
	private final static String body = "Hello World";
	private static final RestTemplate restTemplate = new RestTemplate();
	// cannot initialize too early ?
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		base_url = "http://localhost:" + serverPort + route;
	}

	@Test
	public void test1() throws Exception {
		// Assumptions.assumeFalse(false);
		responseEntity = restTemplate.getForEntity(base_url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test2() throws Exception {
		// Assumptions.assumeFalse(false);
		responseEntity = restTemplate.getForEntity(base_url, String.class);
		assertThat(responseEntity.getBody(), containsString(body));
	}

	@Test
	public void test4() throws Exception {
		// Assumptions.assumeFalse(false);
		String name = "value";
		String url = "http://localhost:" + serverPort + "/generate?name=" + name;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(),
				containsString(String.format("hello %s", name)));
	}

	@Disabled("this test is added to fail and let show the response body")
	@Test
	public void test5() throws Exception {
		// Assumptions.assumeFalse(false);
		String name = "value";
		String url = "http://localhost:" + serverPort + "/model";
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(), is(""));
	}

}
