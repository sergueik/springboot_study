package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.is;

import example.controller.Controller.Data;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.controller.RegexpValidationController.Result;

// NOTE: incomplete: requires explicit start of the app in the sibling console otherwise having
// org.springframework.web.client.ResourceAccessException: I/O error on POST request for
// http://localhost:8085/basic/post/
// Connection refused: connect; nested exception is java.net.ConnectException: Connection refused: connect
public class RegexpValidationControllerRestTemplateTest {

	public static final String url = "http://localhost:8085/validate";

	private RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<Map> responseEntity = null;
	private ResponseEntity<String> responseEntityString = null;
	private HttpEntity<Map<String, String>> requestEntity = null;
	private HttpEntity<String> requestEntityString = null;
	private Map<String, String> data = new HashMap<>();

	private static Gson gson = new GsonBuilder().create();
	private static final String defaultName = "Hello basic";
	private final String name = "elton";
	private Map<String, Object> response = new HashMap<>();
	private Map<String, Object> resultMap = new HashMap<>();
	private Result result;

	@Before
	public void setUp() {
		// NOTE: headers is required
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		// alternatively supply raw header
		// headers.add("Content-Type", "application/x-www-form-urlencoded");
		data = new HashMap<>();
	}

	@SuppressWarnings("unchecked")
	@Test
	public void test1() {

		requestEntityString = new HttpEntity<String>(
				"expression=Lorem ipsum dolor sit amet", headers);
		responseEntityString = restTemplate.postForEntity(url, requestEntityString,
				String.class, headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Map.class),
				notNullValue());
		System.err.println("Response: " + responseEntityString.getBody());
		response = gson.fromJson(responseEntityString.getBody(), Map.class);
		assertThat(response, notNullValue());
		assertThat(response.get("status"), is("OK"));
		assertThat(response.get("result"), is(""));
	}

	@Test
	public void test2() {

		requestEntityString = new HttpEntity<String>(
				"expression=Lorem \\ipsum dolor sit amet", headers);
		responseEntityString = restTemplate.postForEntity(url, requestEntityString,
				String.class, headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Map.class),
				notNullValue());
		System.err.println("Response: " + responseEntityString.getBody());

		response = gson.fromJson(responseEntityString.getBody(), Map.class);
		assertThat(response.get("status"), is("error"));
		resultMap = (Map) response.get("result");
		System.err.println("Result keys: " + resultMap.keySet());

		assertThat(response, notNullValue());
		result = new Result(resultMap.get("expression").toString(),
				resultMap.get("character").toString(),
				resultMap.get("message").toString(),
				((Double) resultMap.get("index")).intValue());
		assertThat(result.getIndex(), is(7));
		try {
			result = (Result) resultMap;
			assertThat(result.getIndex(), is(7));
		} catch (Exception e) {
			System.err.println("Excepion: " + e.toString());
			// java.lang.ClassCastException: com.google.gson.internal.LinkedTreeMap
			// cannot be cast to example.controller.RegexpValidationController$Result
		}
		try {
			result = gson.fromJson(response.get("result").toString(), Result.class);
		} catch (Exception e) {
			// Excepion: com.google.gson.JsonSyntaxException:
			// com.google.gson.stream.MalformedJsonException: Unterminated object at
			// line 1 column 20 path $.expression
			System.err.println("Excepion: " + e.toString());
		}
	}

	// NOTE: redundant
	@Test
	public void test3() {

		requestEntityString = new HttpEntity<String>(
				"expression=Lorem **ipsum dolor sit amet", headers);
		responseEntityString = restTemplate.postForEntity(url, requestEntityString,
				String.class, headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Map.class),
				notNullValue());
		System.err.println("Response: " + responseEntityString.getBody());

		response = gson.fromJson(responseEntityString.getBody(), Map.class);
		assertThat(response.get("status"), is("error"));
		resultMap = (Map) response.get("result");
		System.err.println("Result keys: " + resultMap.keySet());

		assertThat(response, notNullValue());
		result = new Result(resultMap.get("expression").toString(),
				resultMap.get("character").toString(),
				resultMap.get("message").toString(),
				((Double) resultMap.get("index")).intValue());
		assertThat(result.getIndex(), is(7));
	}

	@Ignore
	// Writing [{expression=Lorem ipsum dolor sit amet}] using
	// [org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
	// leads to org.springframework.web.client.HttpClientErrorException: 415 null
	@SuppressWarnings("unchecked")
	@Test
	public void test4() {
		// same error also with
		// headers.setContentType(MediaType.TEXT_PLAIN);

		data.put("expression", "Lorem ipsum dolor sit amet");
		response = restTemplate.postForObject(url, data, Map.class);
		assertThat(response, notNullValue());
		assertThat(responseEntity.getStatusCode(),
				is(HttpStatus.UNSUPPORTED_MEDIA_TYPE));

	}

	@Ignore
	// Writing [{expression=Lorem ipsum dolor sit amet}] using
	// [org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
	// leads to org.springframework.web.client.HttpClientErrorException: 415 null
	@Test
	public void test5() {

		data.put("expression", "Lorem ipsum dolor sit amet");
		responseEntityString = restTemplate.postForEntity(url, data, String.class,
				headers);

		assertThat(responseEntityString, notNullValue());
		assertThat(responseEntityString.getStatusCode(), is(HttpStatus.OK));
		assertThat(gson.fromJson(responseEntityString.getBody(), Map.class),
				notNullValue());
		System.err.println("Response: " + responseEntityString.getBody());
		response = gson.fromJson(responseEntityString.getBody(), Map.class);
		assertThat(response, notNullValue());
		assertThat(response.get("status"), is("OK"));
		assertThat(response.get("result"), is(""));
	}

}
