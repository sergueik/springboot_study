package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.mockito.Mockito;
import org.springframework.http.ResponseEntity;

import example.controller.ExampleController;
import example.controller.ExampleController.Data;
import example.service.ExampleService;

import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.any;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.containsString;
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
	public void test1() {
		assertThat(controller.hello(), is(body));
	}

	@Test
	public void test2() {

		final List<UUID> uuids = Arrays.asList(
				UUID.fromString("3dd25fab-b689-4693-a589-625a637d10a7"),
				UUID.fromString("b3901787-1396-47e8-aa4f-6f5ae74b887a"));
		ResponseEntity<String> result = controller.list(uuids);
		String responseBody = result.getBody();
		uuids.stream()
				.forEach(o -> assertThat(responseBody, containsString(o.toString())));
	}

}
