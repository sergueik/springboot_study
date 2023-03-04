package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
public class PartialFileDownloadTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int randomServerPort = 8086;

	private final static String route = "/basic/partialupload";
	private String url = null;
	private String data = "1234567890";
	private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.set("Range", "range=10-19");
	}

	@Test
	public void test1() throws Exception {
		url = "http://localhost:" + randomServerPort + route;
		request = new HttpEntity<String>(null, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.PARTIAL_CONTENT));
		assertThat(responseEntity.getBody(), containsString(data));
	}

}
