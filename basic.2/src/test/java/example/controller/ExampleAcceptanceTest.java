package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */
// import org.junit.Before;

// import org.junit.Test;

import org.junit.jupiter.api.BeforeEach;
// import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.springframework.http.HttpStatus.OK;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class ExampleAcceptanceTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	// NOTE: property annotations have no effect
	// @Value("${server.port:8085}")
	// NOTE: property annotations have no effect
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8085;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests

	// ResourceAccess I/O error on GET request:
	// org.springframework.web.client.ResourceAccessException:
	// I/O error on GET request for "http://localhost:0/basic":
	// connect: Address is invalid on local machine, or
	// port is not valid on remote machine

	private final String route = "/basic";
	private final static String body = "Hello basic";
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;
	private final String url = "http://localhost:" + randomServerPort + route;

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test() throws Exception {
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(OK));
		assertThat(responseEntity.getBody(), is(body));
	}

}
