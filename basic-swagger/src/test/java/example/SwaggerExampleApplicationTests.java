package example;

// import org.junit.Before;
// import org.junit.Test;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
// import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.arrayContainingInAnyOrder;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.greaterThan;

// @SpringBootTest(classes = SwaggerExampleApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@SpringBootTest(classes = SwaggerExampleApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })

@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
public class SwaggerExampleApplicationTests {

	@LocalServerPort
	private int port = 8085;

	private static final RestTemplate restTemplate = new RestTemplate();
	private static String route = "/rest/person/xxx";
	private static String body = "xxx";

	@BeforeEach
	public void setUp() {

	}

	
	@Test
	public void test1() throws Exception {
		Assumptions.assumeFalse(false);

		ResponseEntity<String> responseEntity = restTemplate
				.getForEntity("http://localhost:" + port + route, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString(body));
	}
}
