package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.mockito.Mockito;

import example.controller.ExampleController;
import example.controller.ExampleController.Data;
import example.service.ExampleService;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;

public class ServiceUnitTest {

	ExampleController controller;
	ExampleService mockService;
	final static String body = "Hello mock";
	final Data data = new Data("data");

	@BeforeEach
	public void setup() {
		mockService = Mockito.mock(ExampleService.class);
		when(mockService.hello(/* "arguments" */)).thenReturn(body);
		when(mockService.handleData(any(Data.class))).thenReturn(data);
		controller = new ExampleController(mockService);
	}

	@Test
	public void test() {
		assertThat(controller.hello(), is(body));
	}

}
