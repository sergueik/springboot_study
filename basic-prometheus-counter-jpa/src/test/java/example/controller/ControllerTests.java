package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

import io.prometheus.client.CollectorRegistry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class ControllerTests {

	@LocalServerPort
	private int port = 8085;

	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	private static final String counterName = "cpu";

	@Disabled
	@Test
	public void test1() {
		url = "http://localhost:" + port + "/metrics";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		// extra metrics
		List<String> extraMetrics = Arrays.asList(
				String.format("# HELP %s Value of metric from instance", counterName),
				String.format("# TYPE %s gauge", counterName),
				String.format(
						"%s{instance=\"hostname00\",dc=\"dummy\",app=\"instance03\",env=\"application01\",} 6.0",
						counterName));
		// https://stackoverflow.com/questions/80476/how-can-i-concatenate-two-arrays-in-java
		// https://stackoverflow.com/questions/189559/how-do-i-join-two-lists-in-java
		List<String> metrics = new ArrayList<String>();

		metrics.addAll(extraMetrics);
		for (String text : metrics) {
			assertThat(entity.getBody(), containsString(text));
			// TODO: regexp
		}
	}

	// subsequent REST calls do not
	// java.lang.IllegalArgumentException:
	// Collector already registered that provides name:
	// instance_metric_value
	// NOTE: the restTemplate test does not receive that exception,
	// but fail with HttpStatus.INTERNAL_SERVER_ERROR
	@Test
	public void test2() {
		url = "http://localhost:" + port + "/metrics";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test3() {
		url = "http://localhost:" + port + "/metrics";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);

		String entryPattern = String.format("%s\\{"
				+ "instance=\\\"hostname[0-9]+\\\"" + "," + "dc=\\\"\\w+\\\"" + ","
				+ "app=\\\"\\w+\\\"" + "," + "env=\\\"\\w+\\\"," + "\\} [0-9.]+",
				counterName);
		List<String> entries = Arrays.asList(entity.getBody().split("\n")).stream()
				.filter(o -> o.contains(counterName))
				.filter(o -> o.contains("hostname")).collect(Collectors.toList());
		for (String line : entries) {
			assertThat(line, matchesPattern(entryPattern));
			System.err.println("inspected line: " + line);
		}
		// count
		assertThat(entries.size(), is(5));
	}
}