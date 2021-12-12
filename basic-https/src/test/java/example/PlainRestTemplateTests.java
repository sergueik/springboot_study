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
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.yaml")
@SuppressWarnings("deprecation")
public class PlainRestTemplateTests {

	// does not get resolved through @Value annotation
	// @Value("${server.port:8443}")
	private int serverPort = 8443;

	private String url = String.format("https://localhost:%d/employees/",
			serverPort);

	private ResponseEntity<String> responseEntity = null;

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;
	private RestTemplate restTemplate;

	@Before
	public void before() throws Exception {
		restTemplate = new RestTemplate();
	}

	// see also:
	// https://stackoverflow.com/questions/39651097/how-to-add-basic-auth-to-autowired-testresttemplate-in-springboottest-spring-bo
	@Test
	public void test() throws Exception {

		try {
			restTemplate.getInterceptors()
					.add(new BasicAuthorizationInterceptor(username, password));
			System.err.println("Credentials: " + username + "/" + password);
			responseEntity = restTemplate.getForEntity(url, String.class);
		} catch (Exception e) {
			System.err.println("Exception: " + e.toString());
			assertThat(e.getMessage(), containsString(
					"unable to find valid certification path to requested target"));
		}
	}
}
