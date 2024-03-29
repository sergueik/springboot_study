package example.controller;
/**
 * Copyright 2021,2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
public class MultipartFormDataTest {

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
		headers = new HttpHeaders();
		headers.set("Content-Type", "multipart/form-data; boundary=\"boundary\"");
	}

	// @Disabled()
	@Test
	public void test1() throws Exception {
		// NOTE:
		// "http://localhost:" + randomServerPort
		// is not optional
		url = "http://localhost:" + randomServerPort + route
				+ "?operation=send&param=something&servername=localhost";
		//@formatter:off
		body = String.join("\r\n",
				Arrays.asList(
						"--boundary",
						"Content-Disposition: form-data; name=\"file\"; filename=\"temp.txt\"",
						"Content-Type: application/octet-stream", 
						"", 
						data, 
						"",
						"--boundary--", 
						""));
  	//@formatter:on
		request = new HttpEntity<String>(body, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString(data));
	}

	@Test
	public void test2() throws Exception {
		// NOTE:
		// "http://localhost:" + randomServerPort
		// is not optional
		url = "http://localhost:" + randomServerPort + route;
		// NOTE: FRAGILE! Removing empty elements leads to Response Status 400
		// org.springframework.web.bind.MissingServletRequestParameterException:
		// Required String parameter 'operation' is not present
		//@formatter:off
		body = String.join("\r\n",
				Arrays.asList("--boundary",
						"Content-Disposition: form-data; name=\"param\"", 
						"", 
						"some value",
						"--boundary", 
						"Content-Disposition: form-data; name=\"operation\"",
						"", 
						"send", 
						"--boundary", 
						"Content-Disposition: form-data; name=\"servername\"",
						"", 
						"localhost", 
						"--boundary",
						"Content-Disposition: form-data; name=\"file\"; filename=\"data.txt\"",
						"Content-Type: application/octet-stream", 
						"", 
						"Line1: one",
						"Line2: two", 
						"Line3: three", 
						"", 
						"--boundary--", 
						""
						));
  	//@formatter:on
		System.err.println("POST body: " + body);
		request = new HttpEntity<String>(body, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString("one"));
	}

	private List<String> buildPayload(Map<String, String> params) {

		List<String> payload = new ArrayList<>();
		for (Entry<String, String> e : params.entrySet()) {
			payload
					.addAll(
							Arrays
									.asList(new String[] { "--boundary",
											String.format(
													"Content-Disposition: form-data; name=\"%s\"",
													e.getKey()),
											"", e.getValue() }));
		}
		return payload;
	}

	@Test
	public void test3() throws Exception {
		Map<String, String> params = new HashMap<>();
		params.put("operation", "send");
		params.put("param", "something");
		params.put("servername", "localhost");

		// NOTE:
		url = "http://localhost:" + randomServerPort + route;
		List<String> payload = buildPayload(params);
		//@formatter:off
		payload.addAll(Arrays.asList(
				"--boundary",
				"Content-Disposition: form-data; name=\"file\"; filename=\"temp.txt\"",
				"Content-Type: application/octet-stream", 
				"", 
				data, 
				"",
				"--boundary--", 
				""));
  	//@formatter:on
		body = String.join("\r\n", payload);
		request = new HttpEntity<String>(body, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString(data));
	}

}
