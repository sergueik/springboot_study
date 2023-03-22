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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;

public class MockServiceTest {

	Controller controller;
	ExampleService mockService;
	// Default constructor cannot handle exception type URISyntaxException thrown
	// by implicit super constructor. Must define an explicit constructor
	RequestEntity request;
	private final String url = "https://localhost:80/cgi-bin/example.cgi?a%20b%20c";

	@Before
	public void setup() throws URISyntaxException {
		request = new RequestEntity<String>(HttpMethod.GET, new URI(url));
		mockService = Mockito.mock(ExampleService.class);

	}

	@Test
	public void test1() throws URISyntaxException {
		when(mockService.runCGiBINScript(any(String.class), any(String[].class)))
				.thenReturn("called");
		controller = new Controller(mockService);
		assertThat(controller.legacyParam("example.cgi", request), is("called"));
	}

	@Test
	public void test2() throws URISyntaxException {
		// Invalid use of argument matchers
		// When using matchers, all arguments have to be provided by matchers
		when(mockService.runCGiBINScript(any(String.class),
				eq(new String[] { "a", "b", "c" }))).thenReturn("got args");
		controller = new Controller(mockService);
		assertThat(controller.legacyParam("example.cgi", request), is("got args"));
	}
}
