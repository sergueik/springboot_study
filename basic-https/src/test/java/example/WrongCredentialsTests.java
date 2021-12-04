package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import javax.net.ssl.SSLContext;

import org.apache.http.client.HttpClient;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContextBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = example.Launcher.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.properties")
@TestConfiguration
public class WrongCredentialsTests {

	private static final String url = "https://localhost:8443/employees/";

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;
	private String password = "wrong password";

	private RestTemplate customRestTemplate() throws Exception {
		SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(
				trustStore.getURL(), trustStorePassword.toCharArray()).build();
		SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(
				sslContext);
		HttpClient httpClient = HttpClients.custom()
				.setSSLSocketFactory(socketFactory).build();
		HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(
				httpClient);
		return new RestTemplate(factory);
	}

	private RestTemplate restTemplate;

	// NOTE: Disabled attribute does not protect from encountering the error:
	// Web server failed to start. Port 8443 was already in use
	// @Disabled("Disabled...")
	@Test
	public void test3() throws Exception {
		restTemplate = customRestTemplate();
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

	@AfterEach
	public void after() throws Exception {

	}
}
