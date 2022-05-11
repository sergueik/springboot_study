package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */


import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesPattern;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class NodeExporterControllerTests {

	@LocalServerPort
	private int port = 8085;

	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	private static final String counterName = "cpu";

	@Test
	public void test1() {
		url = "http://localhost:" + port + "/metrics";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		// assertThat(entity.getHeaders().get("Content-Type"),
		// is(MediaType.TEXT_PLAIN));
		// standard Prometheus metrics delivered by
		// CollectorRegistry.defaultRegistry.metricFamilySamples
		List<String> defaultMetrics = Arrays.asList(
				"# HELP requests_total Total number of requests.",
				"# TYPE requests_total counter", "requests_total",
				"# HELP requests_latency_seconds Request latency in seconds.",
				"# TYPE requests_latency_seconds histogram");
		// extra metrics
		List<String> extraMetrics = Arrays.asList(
				String.format("# HELP %s Value of metric from instance", counterName),
				String.format("# TYPE %s gauge", counterName),
				String.format(
						"%s{instance=\"hostname00\",datacenter=\"west\",appid=\"database\",environment=\"qa\",} 42.0",
						counterName));
		// https://stackoverflow.com/questions/80476/how-can-i-concatenate-two-arrays-in-java
		// https://stackoverflow.com/questions/189559/how-do-i-join-two-lists-in-java
		List<String> metrics = new ArrayList<String>();
		metrics.addAll(defaultMetrics);
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

		String entryPattern = String
				.format("%s\\{" + "instance=\\\"hostname[0-9]+\\\"" + ","
						+ "datacenter=\\\"\\w+\\\"" + "," + "appid=\\\"\\w+\\\"" + ","
						+ "environment=\\\"\\w+\\\"," + "\\} [0-9.]+", counterName);
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
