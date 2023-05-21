package example.controller;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.stream.Collectors;

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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.internal.LinkedTreeMap;

//based on: https://www.baeldung.com/spring-rest-template-multipart-upload
//
// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8086" })
@PropertySource("classpath:application.properties")
public class MiltipartDownloadRestTemplateTest {

	@LocalServerPort
	private int randomServerPort = 8086;

	private String route = null;
	private Resource resource = null;
	private String url = null;
	private static final RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;
	private MultiValueMap<String, Object> body = null;
	private static List<Map<String, Object>> values = new ArrayList<>();
	private static Map<String, Object> value = new HashMap<>();
	private static Gson gson = new GsonBuilder().serializeNulls()
			.disableHtmlEscaping().setLenient().create();
	List<String> filenames = new ArrayList<>();
	private static String valueCheck = null;
	private static String[] checkResults = new String[] { "fileName",
			"fileDownloadUri", "fileType", "size" };

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		body = new LinkedMultiValueMap<>();
		headers.setContentType(MediaType.MULTIPART_FORM_DATA);
	}

	@Disabled("MethodNotAllowed")
	@Test
	public void test3() {
		route = "/uploadFile";
		url = "http://localhost:" + randomServerPort + route;
		HttpHeaders headers = restTemplate.headForHeaders(url);
		assertThat(headers.get("Accept-Ranges").get(0), containsString("bytes"));
		assertThat(headers.getContentLength(), greaterThan(0L));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void test1() throws Exception {
		body.clear();
		resource = getTestFile();
		body.add("file", resource);
		HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(
				body, headers);
		route = "/uploadFile";
		url = "http://localhost:" + randomServerPort + route;
		request = new HttpEntity<String>(null, headers);

		responseEntity = restTemplate.postForEntity(url, requestEntity,
				String.class);

		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(),
				containsString(resource.getFilename()));
		assertThat(responseEntity.getBody(),
				containsString(String.format("%d", resource.contentLength())));
		value = gson.fromJson(responseEntity.getBody(), Map.class);
		assertThat(value.keySet(), containsInAnyOrder(checkResults));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void test2() throws Exception {

		body.clear();
		for (int cnt = 0; cnt != 3; cnt++) {
			resource = getTestFile();
			body.add("files", resource);
			filenames.add(resource.getFilename());
		}

		HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(
				body, headers);
		route = "/uploadMultipleFiles";
		url = "http://localhost:" + randomServerPort + route;
		request = new HttpEntity<String>(null, headers);

		responseEntity = restTemplate.postForEntity(url, requestEntity,
				String.class);

		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));

		values = gson.fromJson(responseEntity.getBody(), List.class);
		value = values.get(0);
		assertThat(value.keySet(), containsInAnyOrder(checkResults));
		// valueCheck = (String) value.get("fileName");
		// assertThat(valueCheck, is(filenames.get(0)));
		Set<Object> filenamesReturned = values.stream()
				.map((Map<String, Object> o) -> o.get("fileName"))
				.collect(Collectors.toSet());
		Object[] filenamesArray = new Object[filenames.size()];
		filenames.toArray(filenamesArray);

		assertThat(filenamesReturned, containsInAnyOrder(filenamesArray));
	}

	public static Resource getTestFile() throws IOException {
		Path testFile = Files.createTempFile("test-file", ".txt");
		System.err.println("Creating and Uploading Test File: " + testFile);
		Files.write(testFile, "Test file".getBytes());
		return new FileSystemResource(testFile.toFile());
	}

}
