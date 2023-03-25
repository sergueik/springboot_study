package example.controller;

import org.mockito.Mockito;
import org.springframework.http.HttpMethod;
import org.springframework.http.RequestEntity;

import example.controller.Controller;
import example.service.ExampleService;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class MockPostServiceTest {

	Controller controller;
	ExampleService mockService;
	// Default constructor cannot handle exception type URISyntaxException thrown
	// by implicit super constructor. Must define an explicit constructor

	private final String url = "https://localhost:80/cgi-bin/example.cgi?a%20b%20c";
	// see also:
	// https://www.tabnine.com/code/java/methods/org.springframework.http.RequestEntity/post
	private final String body = "{ \"foo\":  \"bar\" }";

	@Before
	public void setup() throws URISyntaxException {
		mockService = Mockito.mock(ExampleService.class);
	}

	// @Ignore
	@Test
	public void test1() throws URISyntaxException {
		RequestEntity<byte[]> request;
		// Invalid use of argument matchers
		// When using matchers, all arguments have to be provided by matchers
		request = RequestEntity.post(new URI(url))
				.body(body.getBytes(StandardCharsets.UTF_8));
		when(mockService.runCGiBINScript(any(String.class), eq(body)))
				.thenReturn("got json");
		controller = new Controller(mockService);
		assertThat(controller.status("status1.cgi", (byte[]) request.getBody()),
				is("got json"));
	}

	// @Ignore
	@Test
	public void test2() throws URISyntaxException {
		RequestEntity<String> request;
		// Invalid use of argument matchers
		// When using matchers, all arguments have to be provided by matchers
		request = RequestEntity.post(new URI(url)).body(body);
		when(mockService.runCGiBINScript(any(String.class), eq(body)))
				.thenReturn("got json");
		controller = new Controller(mockService);
		assertThat(controller.status("status.cgi", request.getBody()),
				is("got json"));
	}
}
