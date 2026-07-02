package example.controller;

import com.google.gson.Gson;
/**
 * Copyright 2026 Serguei Kouzmine
 */
import com.google.gson.GsonBuilder;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;
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

import org.springframework.boot.web.server.LocalServerPort;
// import org.springframework.boot.test.web.server.LocalServerPort;
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

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import example.dto.FinalizeRequest;

import org.slf4j.LoggerFactory;
import org.slf4j.event.LoggingEvent;

import me.desair.tus.server.HttpMethod;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(OrderAnnotation.class)
public class FinalizeControllerRestTemplateTest {

	@LocalServerPort
	private int randomServerPort = 8086;
	private final String uploadId = "fa856d03-f385-466a-9c5b-b3cebd532542";
	private final static String route = "/api/uploads/finalize";
	private String url = null;
	private String location = null;

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
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	ResponseEntity<String> response = null;

	@Test
	@Order(1)
	public void test1() throws Exception {

		url = "http://localhost:" + randomServerPort + route;
		byte[] body = String.format("{\"uploadId\":\"%s\"}", uploadId).getBytes();
		HttpEntity<byte[]> entity = new HttpEntity<>(body, headers);
		response = restTemplate.postForEntity(url, entity, String.class);
		assertThat(response.getStatusCode(), is(HttpStatus.CREATED));
	}

	private static final Gson gson = new Gson();

	@DisplayName("This server does report upload in progress")
	@Test
	@Order(2)
	void test2() throws Exception {

		HttpEntity<String> entity = new HttpEntity<>(gson.toJson(new FinalizeRequest(uploadId)), headers);
		response = restTemplate.postForEntity(url, entity, String.class);
		assertThat(response.getStatusCode(), is(HttpStatus.CREATED));
	}

}
