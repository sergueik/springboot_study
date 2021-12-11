package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.support.BasicAuthorizationInterceptor;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8443" })
@PropertySource("classpath:application.properties")
@SuppressWarnings("deprecation")
public class BasicAuthTests {

	// does not get resolved through @Value annotation
	// @Value("${server.port:8443}")
	private int serverPort = 8443;

	private String url = String.format("https://localhost:%d/employees/", serverPort);

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;
	private RestTemplate restTemplate;

	@Before
	public void before() throws Exception {
		restTemplate = CustomRestTemplateHelper.customRestTemplate(trustStore, trustStorePassword);
	}

	// see also:
	// https://stackoverflow.com/questions/39651097/how-to-add-basic-auth-to-autowired-testresttemplate-in-springboottest-spring-bo
	@Test
	public void test2() throws Exception {
		restTemplate.getInterceptors().add(new BasicAuthorizationInterceptor(username, password));
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		assertThat(responseEntity.getBody(), containsString("{}"));

	}

	@Test
	public void test1() throws Exception {

		try {
			responseEntity = restTemplate.getForEntity(url, String.class);
			// the following line is not reached, but keep the assertion
			assertThat(responseEntity.getStatusCode(), is(HttpStatus.UNAUTHORIZED));
		} catch (Exception e) {
			System.err.println("Exception: " + e.toString());
			assertThat(e.getMessage(), containsString("Full authentication is required to access this resource"));
		}
	}
}
