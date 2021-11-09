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
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

// Caused by: org.springframework.context.ApplicationContextException: Unable to start embedded container; nested exception is org.springframework.context.ApplicationContextException: Unable to start EmbeddedWebApplicationContext due to missing EmbeddedServletContainerFactory bean.
@RunWith(SpringRunner.class)
@SpringBootTest(
		/* classes = de.consol.RestServiceDemo.RestServiceDemoApplicationTests.class,  */
		webEnvironment = WebEnvironment.RANDOM_PORT, properties = {
				"serverPort=8080" })
@PropertySource("classpath:application.properties")
public class ApplicationTests {

	@Autowired
	private TestRestTemplate restTemplate;

	@Test
	public void contextLoads() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/hello-world",
				String.class);
		assertThat(entity.getStatusCode()).isEqualTo(HttpStatus.OK);
	}

	@Value("${serverPort:8080}")
	private int managementPort;

	@Test
	public void metrics() {
		ResponseEntity<String> entity = restTemplate.getForEntity("/metrics",
				String.class);
		// ResponseEntity<String> entity = restTemplate.getForEntity(
		// "http://localhost:{port}/metrics", String.class, managementPort);
		assertThat(entity.getStatusCodeValue()).isEqualTo(200);
		assertThat(entity.getBody()).contains("# HELP heap_used heap_used");
		assertThat(entity.getBody()).contains("# TYPE heap_used gauge");

		assertThat(entity.getBody())
				.contains("# HELP requests_total Total number of requests.");
		assertThat(entity.getBody()).contains("# TYPE requests_total counter");
		assertThat(entity.getBody()).contains("requests_total"); // TODO: regexp
	}

}
