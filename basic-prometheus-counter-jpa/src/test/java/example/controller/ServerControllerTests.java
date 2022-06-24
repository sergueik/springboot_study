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
public class ServerControllerTests {

	@LocalServerPort
	private int port = 8085;

	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	private static final String hostnamePart = "hostname0";
	private Collection<String> jsonEntries;
	private String page = null;
	private final static List<String> hostnames = Arrays.asList("hostname00",
			"hostname01", "hostname02", "hostname05");

	private final static List<String> propertyNames = Arrays.asList("serverId",
			"serverName");

	private static ResponseEntity<String> entity;

	@BeforeEach
	public void before() {
		url = "http://localhost:" + port + "/server?server=" + hostnamePart;
		entity = restTemplate.getForEntity(url, String.class);
		page = entity.getBody();
		// System.err.println("Examine page: " + page);
		// JSON:
		// [{"serverId":101,"serverName":"hostname00"},{"serverId":102,"serverName":"hostname01"},{"serverId":103,"serverName":"hostname02"},{"serverId":105,"serverName":"hostname05"}]
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

	// Count Values

	@Test
	public void test3() throws InvalidPathException {
		int length = JsonPath.read(page, "$.length()");
		assertThat("Unexpected count of rows: " + length, length,
				is(hostnames.size()));
	}

	// https://stackoverflow.com/questions/9291435/parse-json-array-file-with-jsonpath
	@Test
	public void test4() throws InvalidPathException {
		jsonEntries = JsonPath.read(page, "$[0].keys()");
		assertThat("Unexpected columns in JSON: " + jsonEntries, jsonEntries.size(),
				is(propertyNames.size()));
		for (String propertyName : propertyNames) {
			// TODO: regexp ?
			assertThat(String.format("missing hostname %s", propertyName),
					jsonEntries, hasItem(propertyName));
			assertThat(JsonPath.read(page, String.format("$[0].%s", propertyName)),
					notNullValue());
		}
	}

	@Test
	public void test5() throws InvalidPathException {
		jsonEntries = JsonPath.read(page, "$[*]");
		assertThat("Unexpected values in JSON: " + jsonEntries, jsonEntries.size(),
				is(4));
	}

	@Test
	public void test6() throws InvalidPathException {
		jsonEntries = JsonPath.read(page, "$.*.serverName");
		assertThat("Unexpected data values in JSON: " + jsonEntries,
				jsonEntries.size(), is(hostnames.size()));
	}
}
