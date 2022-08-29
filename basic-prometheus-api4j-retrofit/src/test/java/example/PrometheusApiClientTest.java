package example;

import org.junit.Before;
import org.junit.Test;

import example.PrometheusApiClient;
import example.models.KeyValResponse;
import example.models.MatrixResponse;
import example.models.PrometheusResponse;
import example.models.VectorResponse;

import java.io.IOException;
import example.PrometheusApiClient;

// NOTE: the test assumes prometheus running locally
public class PrometheusApiClientTest {
	private PrometheusApiClient client;
	Long now;
	Long tenMinEarlier;
	private String prometheus = System.getProperty("prometheus", "localhost");

	@Before
	public void initTest() {
		client = new PrometheusApiClient(
				String.format("http://%s:9090/", prometheus));
		now = System.currentTimeMillis() / 1000;
		tenMinEarlier = now - 10 * 60;
	}

	@Test
	public void testQuery() throws IOException {
		VectorResponse response = client.query("go_gc_duration_seconds");
		System.out.println("response = " + response);
	}

	@Test
	public void testQueryWithTime() throws IOException {
		VectorResponse response = client.query("go_gc_duration_seconds",
				"2018-06-01T20:10:51.781Z");
		System.out.println("response = " + response);
	}

	@Test
	public void testQueryWithTimestamp() throws IOException {
		VectorResponse response = client.query("go_gc_duration_seconds",
				tenMinEarlier.toString());
		System.out.println("response = " + response);
	}

	@Test
	public void testQueryWithTimeout() throws IOException {
		VectorResponse response = client.query("go_gc_duration_seconds", "", "1ms");
		System.out.println("response = " + response);
	}

	@Test
	public void testQueryRange() throws IOException {
		MatrixResponse response = client.queryRange("go_gc_duration_seconds",
				tenMinEarlier.toString(), now.toString(), "5m", "");
		System.out.println("response = " + response);
	}

	@Test
	public void testFindSeries() throws IOException {
		KeyValResponse response = client.findSeries("up", tenMinEarlier.toString(),
				now.toString());
		System.out.println("response = " + response);
	}
}