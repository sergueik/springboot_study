package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsInAnyOrder;

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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.internal.LinkedTreeMap;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
public class MultipartFormDataTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int randomServerPort = 8086;
	private static Gson gson = new GsonBuilder().serializeNulls()
			.disableHtmlEscaping().setLenient().create();

	private final static String data = "test data";
	private final static String boundary = "data_boundary";
	private final static String route = "/uploadFile";
	private static String body = null;
	private String url = null;
	private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;
	private static Map<String, Object> value = new HashMap<>();
	private static String valueCheck = null;
	private static String[] checkResults = new String[] { "fileName",
			"fileDownloadUri", "fileType", "size" };


	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.set("Content-Type", "multipart/form-data; boundary=\"boundary\"");
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
		final String fileName = "temp.txt";
		url = "http://localhost:" + randomServerPort + route;
		List<String> payload = buildPayload(params);
		//@formatter:off
		payload.addAll(Arrays.asList(
				"--boundary",
				String.format("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", fileName),
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

		value = gson.fromJson(responseEntity.getBody(), Map.class);

		assertThat(value.keySet(), containsInAnyOrder(checkResults));
		valueCheck = (String) value.get("fileName");
		assertThat(valueCheck, is(fileName));
	}

}
