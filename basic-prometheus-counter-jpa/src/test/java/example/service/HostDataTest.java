package example.service;

/**
 * Copyright 2022 Serguei Kouzmine
 */
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import example.service.HostData;

public class HostDataTest {

	private HostData hostData = null;
	private List<String> metrics = new ArrayList<>();
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricExtractors = new HashMap<>();
	private final String hostname = "hostname00";

	@BeforeEach
	public void before() {
		hostData = new HostData(hostname);
	}

	@Test
	public void test1() throws Exception {

		metrics.add("cpu");
		metrics.add("memory");
		metrics.add("rpm");
		metrics.add("disk");
		metrics.add("missing data");
		// the fifth element
		metricExtractors.put("load_average",
				"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");
		hostData.setMetrics(metrics);
		hostData.setMetricExtractors(metricExtractors);

		hostData.readData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		// the "missing data" key does not count
		assertThat(data.keySet().size(),
				is(metrics.size() + metricExtractors.keySet().size() - 1));

		// NOTE: loading all metrics as strings
		assertThat(data.get("memory"), is("20"));
		assertThat(data.containsKey("missing data"), is(false));
		assertThat(data.containsKey("dummy"), is(false));
	}

	@Disabled("metric extractors temorarily appears broken")
	@Test
	public void test2() throws Exception {

		metricExtractors.put("disk", "([0-9.]+)");
		hostData.setMetricExtractors(metricExtractors);
		hostData.readData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(1));
		// NOTE: loading all metrics as strings
		assertThat(data.get("disk"), is("40.5"));
	}

	@Test
	public void test3() throws Exception {

		hostData.loadData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(6));
		assertThat(data.get("load_average"), is("1 2 3 4 6"));
	}

}
