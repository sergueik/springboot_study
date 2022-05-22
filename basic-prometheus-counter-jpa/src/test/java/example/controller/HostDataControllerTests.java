package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
// import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasItem;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.json.JSONArray;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.InvalidPathException;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class HostDataControllerTests {

	@LocalServerPort
	private int port = 8085;

	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	private static final String hostname = "hostname00";
	private static final String counterName = "cpu";
	private Collection<String> jsonEntries;
	private String page = null;
	private final static List<String> metricNames = Arrays.asList("memory", "cpu",
			"disk", "rpm");
	private static ResponseEntity<String> entity;

	@BeforeEach
	public void before() {
		url = "http://localhost:" + port + "/hostdata/" + hostname;
		entity = restTemplate.getForEntity(url, String.class);
	}

	@Test
	public void test1() {
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test2() {
		assertThat(entity.getHeaders().containsKey("Content-Type"), is(true));
		assertThat(entity.getHeaders().getContentType(),
				is(MediaType.APPLICATION_JSON));
	}

	// JSON:
	// {"hostname":"hostname00","data":{"disk":"40.5","memory":"20","load_average":"6","cpu":"10","rpm":"100"}}
	// Count Values

	@Test
	public void test3() {

		page = entity.getBody();
		// System.err.println("Examine page: " + page);
		try {
			jsonEntries = JsonPath.read(page, "$.keys()");
			assertThat("Unexpected keys in JSON: " + jsonEntries, jsonEntries.size(),
					is(2));
			jsonEntries = JsonPath.read(page, "$.data.keys()");
			assertThat("Unexpected data keys in JSON: " + jsonEntries,
					jsonEntries.size(), is(5));
			for (String metricName : metricNames) {
				assertThat(String.format("missing metric %s", metricName), jsonEntries,
						hasItem(metricName));
				// NOTE not asserting with "contains"
				// [ERROR] HostDataControllerTests.test3:90 missing metric memory
				// Expected: iterable containing ["memory"]
				// but: item 0: was "disk"
				assertThat(JsonPath.read(page, String.format("$.data.%s", metricName)),
						notNullValue());
				// TODO: regexp
			}
			for (String metricName : metricNames) {
				assertThat(JsonPath.read(page, String.format("$.data.%s", metricName)),
						notNullValue());
				// TODO: regexp
			}

		} catch (InvalidPathException e) {
			throw (e);
		}

	}

	@Test
	public void test4() {

		page = entity.getBody();
		// System.err.println("Examine page: " + page);
		try {
			jsonEntries = JsonPath.read(page, "$.*");
			assertThat("Unexpected values in JSON: " + jsonEntries,
					jsonEntries.size(), is(2));
			jsonEntries = JsonPath.read(page, "$.data.*");

			assertThat("Unexpected data values in JSON: " + jsonEntries,
					jsonEntries.size(), is(5));
		} catch (InvalidPathException e) {
			throw (e);
		}

	}
}
