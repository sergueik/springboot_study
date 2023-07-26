package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = Application.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })

@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
public class ApplicationTests {

	@LocalServerPort
	private int port = 8085;

	private static final RestTemplate restTemplate = new RestTemplate();

	private static final String userName = "john";
	private static String route = String.format("/rest/person/%s", userName);

	@BeforeEach
	public void setUp() {

	}

	@Test
	public void test1() throws Exception {
		Assumptions.assumeFalse(false);

		ResponseEntity<String> responseEntity = restTemplate
				.getForEntity("http://localhost:" + port + route, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat("Unexpected response headers for " + route,
				responseEntity.getHeaders().get("Content-Type").get(0),
				is(("application/json")));
		String bodyFragment = String.format("\"name\":\"%s\"", userName);
		// JSON document fragment, omitting the other properties of the Person class
		assertThat(responseEntity.getBody(), containsString(bodyFragment));
	}
}
