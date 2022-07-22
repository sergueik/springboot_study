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
import java.util.Set;

import example.service.HostData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */
public class HostDataTest {

	private List<String> metrics = new ArrayList<>();
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricExtractors = new HashMap<>();
	private final String hostname = "hostname00";

	private HostData hostData = null;

	@BeforeEach
	public void before() {
		hostData = new HostData(hostname);
		hostData.setDebug(true);
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
		assertThat(data.keySet().size(), is(5));
		// NOTE: loading all metrics as strings
		assertThat(data.get("memory"), is("20"));
		assertThat(data.containsKey("missing data"), is(false));
		assertThat(data.containsKey("dummy"), is(false));
	}

	@Test
	public void test2() throws Exception {

		metricExtractors.put("disk", "\\b([0-9.]+)\\b");
		hostData.setMetricExtractors(metricExtractors);
		hostData.setDebug(true);
		hostData.readData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		assertThat(String.format("Found unexpected keys: %s", data.keySet()),
				data.keySet().size(), is(1));
		// NOTE: loading all metrics as strings
		assertThat(data.get("disk"), is("40.5"));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void test3() throws Exception {

		hostData.loadData();
		data = hostData.getData();

		String metricName = "load_average";
		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(7));

		// examine the keys of the data. NOTE: there are less laborous ways to do
		// that in hamcrest
		Set<String> keySet = data.keySet();
		String[] keys = new String[keySet.size()];

		keys = new ArrayList<String>(keySet).toArray(keys);
		Arrays.sort(keys);
		assertThat(String.join("|", keys),
				is("cpu|disk|load_average|memory|rpm|timestamp|uptime"));
		assertThat(data.containsKey(metricName), is(true));
		assertThat(data.get(metricName), is("1 2 3 4 6"));
	}

	@Test
	public void test4() throws Exception {

		metricExtractors.put("load_average",
				"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");
		hostData.setMetricExtractors(metricExtractors);
		Map<String, String> extractedMetricNames = new HashMap<>();
		extractedMetricNames.put("load_average", "cpu_load");
		hostData.setExtractedMetricNames(extractedMetricNames);
		hostData.readData();
		data = hostData.getData();

		String metricName = "cpu_load";
		String dataTag = "load_average";
		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(1));
		assertThat(data.containsKey(metricName), is(true));
		assertThat(data.containsKey(dataTag), is(false));
		assertThat(data.get(metricName), is("6"));
	}

	@Test
	public void test5() throws Exception {

		metricExtractors.put("load_averag.",
				"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");
		hostData.setMetricExtractors(metricExtractors);
		Map<String, String> extractedMetricNames = new HashMap<>();
		extractedMetricNames.put("load_averag.", "cpu_load");
		hostData.setExtractedMetricNames(extractedMetricNames);
		hostData.readData();
		data = hostData.getData();

		String metricName = "cpu_load";
		String dataTag = "load_average";
		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(1));
		assertThat(data.containsKey(metricName), is(true));
		assertThat(data.containsKey(dataTag), is(false));
		assertThat(data.get(metricName), is("6"));
	}

}
