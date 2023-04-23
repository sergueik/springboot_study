package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
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
public class MultipartFormDataHeadersTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int randomServerPort = 8086;

	private final static String data = "test data";
	private final static String boundary = "data_boundary";
	private final static String route = "/basic/upload";
	private static String body = null;
	private String url = null;
	private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
	}

	@Test
	public void test1() throws Exception {
		// NOTE:
		// "http://localhost:" + randomServerPort
		// is not optional
		final String excepionType = "org.springframework.web.client.HttpClientErrorException.UnsupportedMediaType";
		url = "http://localhost:" + randomServerPort + route
				+ "?operation=send&param=something";

		try {
			responseEntity = restTemplate.postForEntity(url, new HttpEntity<String>(""), String.class);
			assertThat(responseEntity.getStatusCode(),
					is(HttpStatus.UNSUPPORTED_MEDIA_TYPE));
		} catch (Exception e) {
			System.err.println(String.format("Exception(expecting %s): %s",
					excepionType, e.toString()));
			assertThat(e.getClass().getCanonicalName(), is(excepionType));
		}
	}

}
