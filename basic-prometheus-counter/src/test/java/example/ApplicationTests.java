package example;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

import com.sun.el.stream.Stream;

import io.prometheus.client.CollectorRegistry;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.ArrayUtils;

// Caused by: org.springframework.context.ApplicationContextException: Unable to start embedded container; nested exception is org.springframework.context.ApplicationContextException: Unable to start EmbeddedWebApplicationContext due to missing EmbeddedServletContainerFactory bean.
@RunWith(SpringRunner.class)
@SpringBootTest(
		/* classes = example.ApplicationTests,  */
		webEnvironment = WebEnvironment.RANDOM_PORT, properties = {
				"serverPort=8080" })
@PropertySource("classpath:application.properties")
public class ApplicationTests {

	@Value("${serverPort:8080}")
	private int managementPort;

	@Autowired
	private TestRestTemplate restTemplate;

	@Test
	public void index() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/",
				String.class);
		assertThat(entity.getStatusCode()).isEqualTo(HttpStatus.OK);
		assertThat(entity.getHeaders().get("Content-Type")
				.equals(MediaType.TEXT_HTML_VALUE));
	}

	@Test
	public void contextLoads() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/hello",
				String.class);
		assertThat(entity.getStatusCode()).isEqualTo(HttpStatus.OK);
	}

	// TODO: explore the
	// EndpointLinksResolver: Exposing 14 endpoint(s) beneath base path
	// '/actuator'
	@Test
	public void prometheus() {
		ResponseEntity<String> entity = restTemplate
				.getForEntity("/actuator/prometheus", String.class);
		// ResponseEntity<String> entity = restTemplate.getForEntity(
		// "http://localhost:{port}/metrics", String.class, managementPort);
		assertThat(entity.getStatusCodeValue()).isEqualTo(200);
		for (String text : Arrays.asList(
				"# HELP jvm_memory_used_bytes The amount of used memory",
				"# TYPE jvm_memory_used_bytes gauge")) {
			assertThat(entity.getBody()).contains(text);

		}
	}

	@Test
	public void metrics1() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/metrics",
				String.class);
		assertThat(entity.getStatusCodeValue()).isEqualTo(200);
		assertThat(entity.getHeaders().get("Content-Type")
				.equals(MediaType.TEXT_PLAIN_VALUE));
		// standard Prometheus metrics delivered by
		// CollectorRegistry.defaultRegistry.metricFamilySamples
		List<String> defaultMetrics = Arrays.asList(
				"# HELP requests_total Total number of requests.",
				"# TYPE requests_total counter", "requests_total",
				"# HELP requests_latency_seconds Request latency in seconds.",
				"# TYPE requests_latency_seconds histogram");
		// extra metrics
		List<String> extraMetrics = Arrays.asList(
				"# HELP instance_metric_value Value of metric from instance",
				"# TYPE instance_metric_value gauge",
				"instance_metric_value{instance=\"hostname\",} 42.0");
		// https://stackoverflow.com/questions/80476/how-can-i-concatenate-two-arrays-in-java
		// https://stackoverflow.com/questions/189559/how-do-i-join-two-lists-in-java
		List<String> metrics = new ArrayList<String>();
		metrics.addAll(defaultMetrics);
		metrics.addAll(extraMetrics);
		for (String text : metrics) {
			assertThat(entity.getBody()).contains(text);
			// TODO: regexp
		}
	}

	// @Ignore
	// NOTE: repeating the REST call to /metrics leads to
	// java.lang.IllegalArgumentException: Collector already registered that
	// provides name: instance_metric_value
	// the test does not receive that exception, but a HTTP status 500
	@Test
	public void metrics2Failing() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/metrics",
				String.class);
		assertThat(entity.getStatusCodeValue()).isEqualTo(500);
	}
}
