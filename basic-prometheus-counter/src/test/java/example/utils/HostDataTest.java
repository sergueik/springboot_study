package example.utils;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

public class HostDataTest {
	private HostData hostData = null;
	private List<String> metrics = new ArrayList<>();
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricTaker = new HashMap<>();

	private String hostname = "hostname00";

	@Test
	public void test1() throws Exception {
		hostData = new HostData(hostname);
		metrics.add("cpu");
		metrics.add("memory");
		metrics.add("rpm");
		metrics.add("disk");
		metrics.add("missing data");
		metrics.add("load_average");
		hostData.setMetrics(metrics);
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
		hostData = new HostData(hostname);
		metricTaker.put("disk", "([0-9.]+)");
		hostData.setMetricTaker(metricTaker);
		hostData.readData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(1));
		// NOTE: loading all metrics as strings
		assertThat(data.get("disk"), is("40.5"));
	}
}
