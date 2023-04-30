package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.junit.Before;
import org.junit.Test;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;

import example.controller.Controller.Data;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

// NOTE: incomplete: requires explicit start of the app in the sibling console otherwise having
// org.springframework.web.client.ResourceAccessException: I/O error on POST request for
// http://localhost:8085/basic/post/
// Connection refused: connect; nested exception is java.net.ConnectException: Connection refused: connect
public class RestTemplateTest {

	public static final String baseUrl = "http://localhost:8085/basic";

	private RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private Data response = null;
	private ResponseEntity<Data> responseEntity = null;
	private ResponseEntity<String> responseEntityString = null;
	private HttpEntity<Data> requestEntity = null;
	private HttpEntity<String> requestEntityString = null;

	private String url = null;
	private static Gson gson = new GsonBuilder().create();
	private static final String defaultName = "Hello basic";
	private final String name = "elton";
	private Data data = null;

	@Before
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		data = new Data();
	}

	@Test
	public void test1() {
		url = baseUrl + "/json";
		response = restTemplate.getForObject(url, Data.class);
		assertThat(response, notNullValue());
	}

	@Test
	public void test2() {
		url = baseUrl + "/post";

		data.setName(name);
		response = restTemplate.postForObject(url, data, Data.class);
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(name));

	}
	// based on:
	// https://github.com/ramazan/Spring-Rest-Template-Example/blob/master/src/test/java/com/ramazan/RestTemplateExamples.java

	@Test
	public void test3() {
		url = baseUrl + "/json";
		response = createRequest(url, true);
		assertThat(response, notNullValue());
	}

	@Test
	public void test4() {
		url = baseUrl + "/post/";

		data.setName(name);
		response = createRequest(url, data, false);
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(name));
	}

	private Data createRequest(String API_URL, Boolean flag) {
		return createRequest(API_URL, null, flag);
	}

	private Data createRequest(String API_URL, Data data, Boolean flag) {
		Data response = null;

		if (flag)
			response = restTemplate.getForObject(API_URL, Data.class);
		else
			response = restTemplate.postForObject(API_URL, data, Data.class);

		System.err.println("Response : " + gson.toJson(response));
		return response;
	}

	@Test
	public void test5() {
		url = baseUrl + "/json";
		responseEntity = restTemplate.getForEntity(url, Data.class, headers);

		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), notNullValue());
		response = responseEntity.getBody();
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(defaultName));

	}

	@Test
	public void test6() {
		url = baseUrl + "/post";
		data.setName(name);
		requestEntity = new HttpEntity<Data>(data, headers);
		responseEntity = restTemplate.postForEntity(url, requestEntity, Data.class,
				headers);

		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), notNullValue());
		response = responseEntity.getBody();
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(name));

	}

	@Test
	public void test7() {
		url = baseUrl + "/json";
		responseEntityString = restTemplate.getForEntity(url, String.class,
				headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Data.class),
				notNullValue());
		response = gson.fromJson(responseEntityString.getBody(), Data.class);
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(defaultName));
	}

	@Test
	public void test8() {
		url = baseUrl + "/post";
		data.setName(name);
		requestEntityString = new HttpEntity<String>(gson.toJson(data), headers);
		responseEntityString = restTemplate.postForEntity(url, requestEntityString,
				String.class, headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Data.class),
				notNullValue());
		response = gson.fromJson(responseEntityString.getBody(), Data.class);
		assertThat(response, notNullValue());
		assertThat(response.getName(), is(name));

	}

}
