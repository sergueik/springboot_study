package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

// NOTE: placing the test method with bad credentials in the sameclass as with valid makes the latter test fail

@SuppressWarnings("deprecation")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })
@PropertySource("classpath:application.properties")
@TestConfiguration
public class WrongCredentialsParameterizedTests {

	@LocalServerPort
	private int randomServerPort = 8086;

	private final String route = "/employees/";
	private String url = null;
	private ResponseEntity<String> responseEntity = null;

	// NOTE: @Value annotations do not work well with static
	@Value("${test.username}")
	private String username;
	// https://www.baeldung.com/spring-inject-static-field
	private static String username_static;

	@Value("${test.username}")
	public void setNameStatic(String name) {
		WrongCredentialsParameterizedTests.username_static = username;
	}

	private static final RestTemplate restTemplate = new RestTemplate();

	// static disconnected data provider
	// origin: https://www.lenar.io/junit5-dataprovider-analogue-example/
	// see also: https://www.baeldung.com/parameterized-tests-junit-5
	public static Object[][] userData() {
		return new Object[][] { { "", "" }, { "unknown user", "password" }, { username_static, "wrong password" } };

	}

	@ParameterizedTest
	@MethodSource("userData")
	public void test(final String username, final String password) throws Exception {
		url = "http://localhost:" + randomServerPort + route;
		HttpClientErrorException exception = Assertions.assertThrows(HttpClientErrorException.class, () -> {
			restTemplate.getInterceptors().add(new BasicAuthorizationInterceptor(username, password));
			responseEntity = restTemplate.getForEntity(url, String.class);
			// the following line is not reached
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNAUTHORIZED));
		});
		assertThat(exception.getMessage(), containsString("Bad credentials"));
	}

}
