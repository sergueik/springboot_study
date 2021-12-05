package example;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = example.Launcher.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.properties")
public class PlainIntegrationTest {

	private int serverPort = 8443;

	private String url = (String.format("https://localhost:%d/welcome",
			serverPort));

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	private RestTemplate restTemplate;

	@BeforeEach
	public void beforeEach() throws Exception {
		restTemplate = CustomRestTemplateHelper.customRestTemplate(trustStore,
				trustStorePassword);
	}

	@Test
	public void test1() throws Exception {
		ResponseEntity<String> response = restTemplate.getForEntity(url,
				String.class, Collections.emptyMap());
		assertThat(response.getBody(), is("welcome"));
		assertThat(response.getStatusCode(), is(HttpStatus.OK));
	}
}
