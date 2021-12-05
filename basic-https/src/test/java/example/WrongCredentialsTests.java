package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

@SuppressWarnings("deprecation")
@SpringBootTest(classes = example.Launcher.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.properties")

public class WrongCredentialsTests {

	// @Value("${server.port}")
	// initializes with zero
	private int serverPort = 8443;
	private String url = (String.format("https://localhost:%d/employees/",
			serverPort));

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;
	private String password = "wrong password";

	private RestTemplate restTemplate;

	@BeforeEach
	public void beforeEach() throws Exception {
		restTemplate = CustomRestTemplateHelper.customRestTemplate(trustStore,
				trustStorePassword);
	}

	@Test
	public void test3() throws Exception {
		@SuppressWarnings("deprecation")
		HttpClientErrorException exception = Assertions
				.assertThrows(HttpClientErrorException.class, () -> {
					restTemplate.getInterceptors()
							.add(new BasicAuthorizationInterceptor(username, password));
					responseEntity = restTemplate.getForEntity(url, String.class);
					// the following line is not reached
					assertThat(responseEntity.getStatusCode(),
							is(HttpStatus.UNAUTHORIZED));
				});
		assertThat(exception.getMessage(), containsString("Bad credentials"));
	}

}
