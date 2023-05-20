package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

// based on: https://www.baeldung.com/spring-rest-template-multipart-upload
// 
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
public class MiltipartDownloadRestTemplateTest {

	@LocalServerPort
	private int randomServerPort = 8086;

	private final static String route = "/uploadFile";
	private String url = null;
	private String data = "1234567890";
	private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.MULTIPART_FORM_DATA);
	}

	@Disabled("MethodNotAllowed")
	@Test
	public void test3() {
		url = "http://localhost:" + randomServerPort + route;
		HttpHeaders headers = restTemplate.headForHeaders(url);
		assertThat(headers.get("Accept-Ranges").get(0), containsString("bytes"));
		assertThat(headers.getContentLength(), greaterThan(0L));
	}

	public static Resource getTestFile() throws IOException {
		Path testFile = Files.createTempFile("test-file", ".txt");
		System.err.println("Creating and Uploading Test File: " + testFile);
		Files.write(testFile, "Test file".getBytes());
		return new FileSystemResource(testFile.toFile());
	}

	@Test
	public void test1() throws Exception {
		MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
		Resource resource = getTestFile();
		body.add("file", resource);
		HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(
				body, headers);
		url = "http://localhost:" + randomServerPort + route;
		request = new HttpEntity<String>(null, headers);

		responseEntity = restTemplate.postForEntity(url, requestEntity,
				String.class);

		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(),
				containsString(resource.getFilename()));
		assertThat(responseEntity.getBody(),
				containsString(String.format("%d", resource.contentLength())));
	}

}
