package example.controller;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import com.google.gson.Gson;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;
import java.net.URLEncoder;
import static org.hamcrest.Matchers.containsInAnyOrder;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

// based on:
// https://stackoverflow.com/questions/8297215/spring-resttemplate-get-with-parameters
@SuppressWarnings("unchecked")
public class QueryStringControllerTest {

	@LocalServerPort
	// NOTE: property annotations seem to have no effect here
	// @Value("${server.port:8085}")
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8085;
	// NOTE: when not initialized sets set to zero, which leads to crashing tests
	private static String route = null;

	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private ResponseEntity<String> responseEntity = null;
	private final Gson gson = new Gson();
	private String body;
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> params = new HashMap<>();
	private int id = 10;
	private HttpHeaders headers;
	private HttpEntity entity;
	private String urlTemplate;

	@SuppressWarnings("rawtypes")
	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.set("Accept", "application/json");

		route = "/basic/data/" + id;
		params.put("key1", "value 1");
		params.put("key2", "value 2");
		params.put("key3", "value 3");
		url = "http://localhost:" + randomServerPort + route;

		entity = new HttpEntity(headers);

		UriComponentsBuilder uiComponentsBuilder = UriComponentsBuilder
				.fromHttpUrl(url);
		// NOTE: it use stream, cannot do it like this:
		// params.keySet().stream()
		// .forEach(o -> uiComponentsBuilder = uiComponentsBuilder.queryParam(o,
		// "{" + o + "}"));
		// Local variable uiComponentsBuilder defined in an enclosing scope must be
		// final or effectively final
		// https://stackoverflow.com/questions/30736587/builder-pattern-with-a-java-8-stream
		for (String key : params.keySet()) {
			uiComponentsBuilder = uiComponentsBuilder.queryParam(key,
					"{" + key + "}");
		}

		urlTemplate = uiComponentsBuilder.encode().toUriString();
		url = urlTemplate;
		// see also: https://www.baeldung.com/java-url-encoding-decoding
		for (String key : params.keySet()) {
			url = url.replace("{" + key + "}", URLEncoder.encode(params.get(key)));
		}
	}

	@Test
	public void test1() throws Exception {
		responseEntity = restTemplate.exchange(urlTemplate, HttpMethod.PUT, entity,
				String.class, params);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + route, data.containsKey("id"),
				is(true));
		params.keySet().stream()
				.forEach(o -> assertThat("Unexpected response for " + urlTemplate,
						data.containsKey(o), is(true)));
		assertThat("Unexpected id", Integer.parseInt(data.get("id")), is(id));
	}

	@SuppressWarnings("deprecation")
	@Test
	public void test2() throws Exception {
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		body = responseEntity.getBody();
		data = gson.fromJson(body, Map.class);
		assertThat("Unexpected response for " + url, data.containsKey("id"),
				is(true));

		params.keySet().stream()
				.forEach(o -> assertThat("Unexpected response for " + url,
						data.containsKey(o), is(true)));
		assertThat("Unexpected id", Integer.parseInt(data.get("id")), is(id));
	}

	// responseEntity = restTemplate.getForEntity(url, String.class);
}
