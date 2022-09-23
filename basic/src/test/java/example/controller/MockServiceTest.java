package example.controller;

import org.mockito.Mockito;

import example.controller.Controller;
import example.service.ExampleService;

import static org.mockito.Mockito.when;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;

public class MockServiceTest {

	Controller controller;

	@Before
	public void setup() {
		ExampleService mockService = Mockito.mock(ExampleService.class);
		when(mockService.hello(/* "arguments" */)).thenReturn("Hello mock");
		controller = new Controller(mockService);
	}

	@Test
	public void test() {
		assertThat(controller.hello(), is("Hello mock"));
	}

}
