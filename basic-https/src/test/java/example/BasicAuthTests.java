package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.http.HttpStatus;

import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import javax.net.ssl.SSLContext;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;

import org.apache.commons.codec.binary.Base64;
import org.apache.http.client.HttpClient;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContextBuilder;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.properties")
// @TestConfiguration
@SuppressWarnings("deprecation")
public class BasicAuthTests {

	private static final String url = "https://localhost:8443/employees/";

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;

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

	@Test
	void test1() throws Exception {
		RestTemplate restTemplate = customRestTemplate();

		HttpClientErrorException exception = Assertions
				.assertThrows(HttpClientErrorException.class, () -> {
					responseEntity = restTemplate.getForEntity(url, String.class);
					// the following line is not reached
					assertThat(responseEntity.getStatusCode(),
							is(HttpStatus.UNAUTHORIZED));
				});
		assertThat(exception.getMessage(), containsString(
				"Full authentication is required to access this resource"));
	}

	// see also:
	// https://stackoverflow.com/questions/39651097/how-to-add-basic-auth-to-autowired-testresttemplate-in-springboottest-spring-bo
	@Test
	public void test2() throws Exception {
		RestTemplate restTemplate = customRestTemplate();
		restTemplate.getInterceptors()
				.add(new BasicAuthorizationInterceptor(username, password));
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString("{}"));

	}
}
