package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class ActuatorTest {

	@LocalServerPort
	private int port = 8085;

	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	@Test
	public void test() {
		url = "http://localhost:" + port + "/actuator/prometheus";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);
		// ResponseEntity<String> entity = restTemplate.getForEntity(
		// "http://localhost:{port}/metrics", String.class, managementPort);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		for (String text : Arrays.asList(
				"# HELP jvm_memory_used_bytes The amount of used memory",
				"# TYPE jvm_memory_used_bytes gauge")) {
			assertThat(entity.getBody(), containsString(text));
		}
	}

}
