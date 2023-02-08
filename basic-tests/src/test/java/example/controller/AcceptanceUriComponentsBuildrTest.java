package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

public class AcceptanceUriComponentsBuildrTest {

	@LocalServerPort
	private int randomServerPort = 8085;

	private final String route = "/basic";
	private String url = null;
	private static List<Map<String, Object>> data = new ArrayList<>();
	private static Map<String, Object> param = new HashMap<>();

	@BeforeEach
	public void setUp() {

	}

	// see also: https://www.baeldung.com/spring-uricomponentsbuilder
	// to see complex body successfully posted through curl use flag:
	// curl --trace-ascii /dev/stdout http://localhost:8085/basic --data-urlencode
	// 'filter=[{"columnName":"test","condition":"=","value":"Успешно"}]' -H
	// 'Content-Type: application/json'

	@Test
	public void test1() /* throws Exception */ {
		Assumptions.assumeFalse(false);
		url = "http://localhost:" + randomServerPort + "/";
		int amount = 10;
		param.put("foo", null);
		param.put("bar", "string data");
		param.put("number", 100);
		param.put("array", Arrays.asList("a", "b", "c"));
		param.put("localized", "абвгд");

		data.add(param);
		final String uri = UriComponentsBuilder.fromHttpUrl(url).path(route)
				.queryParam("amount", amount).queryParam("data", data).encode().build()
				.normalize().toUriString();
		System.err.println("generated uri: " + uri);
	}
}
