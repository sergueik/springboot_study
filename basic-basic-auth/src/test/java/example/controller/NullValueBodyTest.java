package example.controller;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestConfiguration
@SuppressWarnings("deprecation")
public class NullValueBodyTest {

	@LocalServerPort
	private int randomServerPort = 8086;

	private final String route = "/null";
	private String url = null;
	private ResponseEntity<Data> responseEntity = null;

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;

	private static final RestTemplate restTemplate = new RestTemplate();

	@BeforeEach
	public void setUp() {
		url = "http://localhost:" + randomServerPort + route;
	}

	@Test
	public void test2() throws Exception {
		restTemplate.getInterceptors().add(new BasicAuthorizationInterceptor(username, password));
		responseEntity = restTemplate.postForEntity(url, "{}", Data.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), nullValue());
		if (responseEntity.getBody() == null) {
			System.err.println("detected null body");
		}
	}

	public static class Data {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(String data) {
			name = data;
		}

		public Data(String name) {
			this.name = name;
		}

		public Data() {
		}
	}
}
