package example.controller;

// import org.junit.Before;
// import org.junit.Test;

import org.junit.jupiter.api.BeforeEach;
// import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.springframework.http.HttpStatus.OK;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class ExampleAcceptanceTest {

	@LocalServerPort
	int randomServerPort;

	private RestTemplate restTemplate;
	private String url;

	@BeforeEach
	public void setUp() {
		restTemplate = new RestTemplate();
		url = "http://localhost:" + randomServerPort + "/basic";
	}

	// @Ignore
	// ResourceAccess I/O error on GET request:
	// org.springframework.web.client.ResourceAccessException: I/O error on GET
	// request for "http://localhost:0/basic": connect: Address is invalid on
	// local machine, or port is not valid on remote machine; nested exception is
	// java.net.ConnectException: connect: Address is invalid on local machine, or
	// port is not valid on remote machine
	@Test
	public void test() throws Exception {
		ResponseEntity responseEntity = restTemplate.getForEntity(url,
				String.class);
		assertThat(responseEntity.getStatusCode(), is(OK));
		assertThat(responseEntity.getBody(), is("Hello basic"));
	}

}
