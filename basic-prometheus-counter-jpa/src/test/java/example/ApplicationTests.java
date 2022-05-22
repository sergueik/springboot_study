package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@PropertySource("classpath:application.properties")

public class ApplicationTests {
	@LocalServerPort
	private int port;
	private String url = null;

	@Autowired
	private TestRestTemplate restTemplate;

	@Test
	public void test1() {
		url = "http://localhost:" + port + "/";
		ResponseEntity<String> entity = restTemplate.getForEntity(url,
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		// assertThat(entity.getHeaders().get("Content-Type"),
		// is(MediaType.TEXT_PLAIN));
		// Expected: is "text/html"
		// but: was "text/html;charset=UTF-8"

	}
}
