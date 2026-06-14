package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
// TODO : find relevant version of junit import
// import org.junit.jupiter.api.MethodOrderer.MethodName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
// import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import me.desair.tus.server.HttpMethod;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(OrderAnnotation.class)
public class RestTemplateTusFileUploadControllerTest {

	@LocalServerPort
	private int randomServerPort = 8086;
	private final String payload = "Hello";
	private final static String route = "/api/upload";
	private String url = null;
	private String location = null;
	// NOTE: need to let Spring inject the RestTemplate for the PATCH request to
	// work
	@Autowired
	private RestTemplate restTemplate;

	// private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		System.err.println("RestTemplate class: " + restTemplate.getRequestFactory().getClass());
		headers = new HttpHeaders();
		headers.set("Tus-Resumable", "1.0.0");
		headers.set("Upload-Defer-Length", "1");
	}

	@Test
	@Order(1)
	public void test1() throws Exception {

		url = "http://localhost:" + randomServerPort + route;
		final HttpEntity<String> request = new HttpEntity<String>(null, headers);

		responseEntity = restTemplate.postForEntity(url, request, String.class);

		assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
		assertThat(responseEntity.getBody(), nullValue());
		HttpHeaders headers = responseEntity.getHeaders();
		assertThat(headers, notNullValue());
		final String key = "Location";
		assertThat(headers.containsKey(key), is(true));
		location = headers.getFirst(key);
		System.err.println("Location: " + location);
		// /api/upload/bc026ad3-9268-48af-8b26-17a98af8996b
	}

	@DisplayName("must use exchange() for TUS testing")
	@Test
	@Order(2)
	public void test2() throws Exception {
		url = "http://localhost:" + randomServerPort + location;
		try {
			headers = restTemplate.headForHeaders(url);
		} catch (HttpClientErrorException e) {
			// NOTE: the inner class HttpClientErrorException.PreconditionFailed is not
			// consistent across versions
			assertThat(e.getStatusCode(), is(HttpStatus.PRECONDITION_FAILED));
			System.err.println("Exception: " + e.getMessage());
			// Exception: 412 : [no body]

			// TODO: capture
			// 2026-06-14 13:01:04.937 WARN 16408 --- [nio-8080-exec-2]
			// m.d.tus.server.TusFileUploadService : Unable to process request HEAD
			// http://localhost:8080/api/upload/6d02d4e1-0011-4571-b3fc-702bd64f1a01. Sent
			// response status 412 with message "This server does not support tus protocol
			// version "

			headers = e.getResponseHeaders();
			System.err.println("Response Headers: " + headers.toString());
			assertThat(headers.getFirst("Tus-Resumable"), is("1.0.0"));
			System.err.println("Response Body: " + e.getResponseBodyAsString());
			// empty
			System.err.println("Status Text: " + e.getStatusText());
			// not useful
			// System.err.println("Stack Trace: ");
			// e.printStackTrace();
		}
	}

	@DisplayName("TUS server handles HEAD request")
	@Test
	@Order(3)
	public void test3() throws Exception {
		url = "http://localhost:" + randomServerPort + location;

		// NOTE: challenge with overloads of `exchange`
		// HttpEntity<String> request = new HttpEntity<>(null, headers);
		// ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.HEAD,
		// request, String.class);

		RequestEntity<Void> request = RequestEntity.head(URI.create(url)).header("Tus-Resumable", "1.0.0").build();

		ResponseEntity<String> response = restTemplate.exchange(request, String.class);
		headers = response.getHeaders();
		System.err.println("Response Headers: " + headers.toString());
		assertThat(headers.containsKey("Upload-Offset"), is(true));
		var offset = headers.getFirst("Upload-Offset");
		assertThat(offset, is("0"));
	}

	@DisplayName("TUS server is actually consuming the PATCH body and advancing the offset")
	@Test
	@Order(4)
	public void test4() throws Exception {
		url = "http://localhost:" + randomServerPort + location;
		RequestEntity<byte[]> request = RequestEntity.patch(URI.create(url)).header("Tus-Resumable", "1.0.0")
				.header("Upload-Offset", "0").header("Content-Type", "application/offset+octet-stream")
				.body(payload.getBytes(StandardCharsets.UTF_8));
		ResponseEntity<String> response = restTemplate.exchange(request, String.class);
		headers = response.getHeaders();
		System.err.println("Response Headers: " + headers.toString());

		assertThat(headers.containsKey("Upload-Offset"), is(true));
		var offset = headers.getFirst("Upload-Offset");
		assertThat(offset, is(String.format("%d", payload.length())));
	}

	@DisplayName("TUS server updates Upload-Offset in HEAD request response")
	@Test
	@Order(5)
	public void test5() throws Exception {
		url = "http://localhost:" + randomServerPort + location;

		RequestEntity<Void> request = RequestEntity.head(URI.create(url)).header("Tus-Resumable", "1.0.0").build();

		ResponseEntity<String> response = restTemplate.exchange(request, String.class);
		
		// examine response headers to verify the payload supplied
		// has actually being persisted and the offset is advancing

		headers = response.getHeaders();
		System.err.println("Response Headers: " + headers.toString());
		assertThat(headers.containsKey("Upload-Offset"), is(true));
		var offset = headers.getFirst("Upload-Offset");
		assertThat(offset, is(String.format("%d", payload.length())));
	}
}
