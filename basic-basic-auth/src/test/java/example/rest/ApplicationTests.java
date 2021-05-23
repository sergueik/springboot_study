package example.rest;

import java.net.URI;
import java.net.URISyntaxException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
// notavailable prior to
// import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringRunner.class)
@SpringBootTest
// @SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class ApplicationTests {
	// @LocalServerPort
	static final int randomServerPort = 8080;
	final static String baseUrl = "http://localhost:" + randomServerPort
			+ "/employees/";
	URI uri;
	// Timeout value in milliseconds
	int timeout = 10_000;
	ResponseEntity<String> result;

	public RestTemplate restTemplate;

	@Before
	public void setUp() {
		restTemplate = new RestTemplate(getClientHttpRequestFactory());
	}

	private HttpComponentsClientHttpRequestFactory getClientHttpRequestFactory() {

		HttpComponentsClientHttpRequestFactory clientHttpRequestFactory = new HttpComponentsClientHttpRequestFactory();
		// Connect timeout
		clientHttpRequestFactory.setConnectTimeout(timeout);

		// Read timeout
		clientHttpRequestFactory.setReadTimeout(timeout);
		return clientHttpRequestFactory;
	}

	@Test
	public void test1() throws URISyntaxException {
		uri = new URI(baseUrl);
		result = restTemplate.getForEntity(uri, String.class);

		// Verify request succeed
		Assert.assertEquals(200, result.getStatusCodeValue());
	}

	@Test
	public void test2() throws URISyntaxException {
		uri = new URI(baseUrl);
		result = restTemplate.getForEntity(uri, String.class);

		// Verify response
		Assert.assertEquals(true, result.getBody().contains("employees"));
	}
}
