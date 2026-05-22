package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.springframework.test.context.junit4.SpringRunner;

import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;

import example.dto.UploadRequest;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class JsonRequestBodyUploadControllerTest {

	@LocalServerPort
	private int port;
	private static String filename = "test.txt";

	private RestTemplate restTemplate;
	private HttpHeaders headers;
	private ObjectMapper mapper;
	private Map<String, String> args = new HashMap<>();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	private String filePath = null;

	@Before
	public void setUp() {
		filePath = Paths.get(System.getProperty("user.dir")).resolve("src/test/resources/files").resolve(filename)
				.toAbsolutePath().toString();
		args.put("foo", "alpha");
		args.put("bar", "beta");

		restTemplate = new RestTemplate();
		mapper = new ObjectMapper();
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	@Test
	public void test1() throws Exception {

		UploadRequest request = buildUploadRequest(new File(filePath), args);
		responseEntity = postRequest("http://localhost:" + port + "/upload/binding", request);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
		assertThat(responseEntity.getBody(), containsString("filename"));
	}

	// t UploadManualEndpoint
	@Test
	public void test2() throws Exception {

		UploadRequest request = buildUploadRequest(new File(filePath), args);
		responseEntity = postRequest("http://localhost:" + port + "/upload/manual", request);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
		assertThat(responseEntity.getBody(), containsString("filename"));
	}

	@Test
	public void test3() throws Exception {

		UploadRequest request = new UploadRequest();
		request.setFoo("broken");
		request.setBar("payload");
		request.setFilename("bad.txt");
		request.setContentType("text/plain");

		/*
		 * bypass setter validation to simulate broken external caller
		 */

		String payload = "{" + "\"foo\":\"broken\"," + "\"bar\":\"payload\"," + "\"filename\":\"bad.txt\","
				+ "\"contentType\":\"text/plain\"," + "\"contentBase64\":\"%%%NOTBASE64%%%\"" + "}";
		final String excepionType = "org.springframework.web.client.HttpClientErrorException.BadRequest";
		try {

			HttpEntity<String> entity = new HttpEntity<>(payload, headers);

			System.out.println("posting: " + payload);
			responseEntity = restTemplate.postForEntity("http://localhost:" + port + "/upload/binding", entity,
					String.class);

			assertThat(responseEntity.getStatusCode(), is(HttpStatus.BAD_REQUEST));
		} catch (Exception e) {
			System.err.println(String.format("Exception(expecting %s): %s", excepionType, e.toString()));
			assertThat(e.getClass().getCanonicalName(), is(excepionType));
		}
	}

	@Test
	public void test4() throws Exception {

		UploadRequest request = new UploadRequest();
		request.setFoo("broken");
		request.setBar("payload");
		request.setFilename("bad.txt");
		request.setContentType("text/plain");

		/*
		 * bypass setter validation to simulate broken external caller
		 */

		String payload = "{" + "\"foo\":\"broken\"," + "\"bar\":\"payload\"," + "\"filename\":\"bad.txt\","
				+ "\"contentType\":\"text/plain\"," + "\"contentBase64\":\"%%%NOTBASE64%%%\"" + "}";

		final String excepionType = "org.springframework.web.client.HttpClientErrorException.UnprocessableEntity";
		try {
			HttpEntity<String> entity = new HttpEntity<>(payload, headers);

			System.out.println("posting: " + payload);
			responseEntity = restTemplate.postForEntity("http://localhost:" + port + "/upload/manual", entity,
					String.class);

			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
		} catch (Exception e) {
			System.err.println(String.format("Exception(expecting %s): %s", excepionType, e.toString()));
			assertThat(e.getClass().getCanonicalName(), is(excepionType));
		}
	}

	private UploadRequest buildUploadRequest(File file, Map<String, String> formArguments) throws Exception {

		byte[] bytes = Files.readAllBytes(file.toPath());
		String base64 = Base64.getEncoder().encodeToString(bytes);
		UploadRequest request = new UploadRequest();
		request.setFilename(file.getName());
		request.setContentType("application/octet-stream");
		request.setContentBase64(base64);
		request.setFoo(formArguments.get("foo"));
		request.setBar(formArguments.get("bar"));
		return request;
	}

	private ResponseEntity<String> postRequest(String endpoint, UploadRequest request) throws Exception {
		String payload = mapper.writeValueAsString(request);
		HttpEntity<String> entity = new HttpEntity<>(payload, headers);
		return restTemplate.postForEntity(endpoint, entity, String.class);
	}

}