package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.lang.reflect.Type;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.dto.UploadRequest;
import example.dto.UploadRequestSerializer;

@SpringBootApplication
@SuppressWarnings("unused")
public class Application implements CommandLineRunner {
	private static final Logger logger = LoggerFactory.getLogger(Application.class);
	private static boolean debug = false;
	private final static boolean directConvertrsion = true;

	private static Gson gson = directConvertrsion ? new Gson()
			: new GsonBuilder().registerTypeAdapter(UploadRequest.class, new UploadRequestSerializer()).create();

	private RestTemplate restTemplate;
	private HttpHeaders headers;
	private ObjectMapper mapper;
	private Map<String, String> argsMap = new HashMap<>();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;

	private String filePath = null;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@Override
	public void run(String... args) throws Exception {
		Map<String, String> cli = parseArgs(args);
		String filename = "test.txt";
		int port = 8085;

		if (cli.containsKey("debug")) {
			debug = true;
		}
		if (debug)
			System.err.println(cli.keySet());
		if (cli.containsKey("help")) {
			System.err.println(String.format("Usage: %s -filename <filename> -port <port>", "jar"));
			return;
		}
		if (cli.containsKey("filename"))
			filename = cli.get("filename");
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		if (filename == null) {
			System.err.println("Missing required argument: filename");
			return;
		}
		System.err.println("Uploading: " + filename);

		filePath = Paths.get(System.getProperty("user.dir")).resolve(filename).toAbsolutePath().toString();
		argsMap.put("foo", "alpha");
		argsMap.put("bar", "beta");

		restTemplate = new RestTemplate();
		mapper = new ObjectMapper();
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);

		UploadRequest request = buildUploadRequest(new File(filePath), argsMap);
		logger.info("Posting: {}", gson.toJson(request));
		responseEntity = postRequest("http://localhost:" + port + "/upload/binding", request);
		logger.info("Response: {}", responseEntity.getBody()); // response is text/plain
		return;
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

	private static Map<String, String> parseArgs(String[] args) {
		if (Arrays.asList(args).contains("debug"))
			System.err.println("Processing: " + Arrays.asList(args));
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
	
}
