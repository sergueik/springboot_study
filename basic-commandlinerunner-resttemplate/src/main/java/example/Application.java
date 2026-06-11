package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import example.config.GlobalProperties;
import example.config.ApplicationProperties;
import example.dto.UploadRequest;

@SpringBootApplication
public class Application implements CommandLineRunner {
	private static final Logger logger = LoggerFactory.getLogger(Application.class);

	private int port = 8085;

	private static String filename = "test.txt";
	private RestTemplate restTemplate;
	private HttpHeaders headers;
	private ObjectMapper mapper;
	private Map<String, String> argsMap = new HashMap<>();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	private String filePath = null;

	@Autowired
	private ApplicationProperties applicationProperties;

	@Autowired
	private GlobalProperties globalProperties;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	// required to implement CommandLineRunner
	@Override
	public void run(String... args) throws Exception {
		// System.out.println(globalProperties);
		// System.out.println(applicationProperties);
		filePath = Paths.get(System.getProperty("user.dir")).resolve(filename).toAbsolutePath().toString();
		argsMap.put("foo", "alpha");
		argsMap.put("bar", "beta");

		restTemplate = new RestTemplate();
		mapper = new ObjectMapper();
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);

		UploadRequest request = buildUploadRequest(new File(filePath), argsMap);
		logger.info("Posting: {}", request);
		responseEntity = postRequest("http://localhost:" + port + "/upload/binding", request);
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
