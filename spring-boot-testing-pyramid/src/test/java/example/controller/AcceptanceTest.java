package example.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.http.HttpStatus.OK;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@PropertySource("classpath:application.properties")
class AcceptanceTest {

	@LocalServerPort
	int randomServerPort;

	private RestTemplate restTemplate;
	private String url;

	@BeforeEach
	void setUp() {
		restTemplate = new RestTemplate();
		url = "http://localhost:" + randomServerPort + "/basic";
	}

	@Test
	void shouldGetDefaultWelcomeMessage() throws Exception {
		ResponseEntity responseEntity = restTemplate.getForEntity(url,
				String.class);
		assertEquals(OK, responseEntity.getStatusCode());
		assertEquals("Hello basic", responseEntity.getBody());
	}
}
