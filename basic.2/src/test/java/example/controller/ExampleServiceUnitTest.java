package example.controller;

import org.mockito.Mockito;

import example.controller.ExampleController;
import example.service.ExampleService;

import static org.mockito.Mockito.when;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;

public class ExampleServiceUnitTest {

	ExampleController controller;
	ExampleService mockService;
	final static String body = "Hello mock";

	@BeforeEach
	public void setup() {
		mockService = Mockito.mock(ExampleService.class);
		when(mockService.hello(/* "arguments" */)).thenReturn(body);
		controller = new ExampleController(mockService);
	}

	@Test
	public void test() {
		assertThat(controller.hello(), is(body));
	}

}
