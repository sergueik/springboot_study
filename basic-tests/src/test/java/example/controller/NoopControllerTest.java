package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.service.ExampleService;

@SpringBootTest
@SuppressWarnings("unchecked")
public class NoopControllerTest {

	// @Autowired
	// ApplicationContext context;

	@Autowired
	@InjectMocks
	// NOTE: incommenting leads to massive exceptio stack and VM crash
	// see also: https://www.baeldung.com/mockito-verify
	NoopController controller;

	private Map<String, String> data = new HashMap<>();
	private Map<String, String> response = new HashMap<>();

	@MockBean
	ExampleService service;

	private static String name = "name";
	final static String body = "Hello";
	private static Gson gson = new GsonBuilder().create();

	private String responseString;

	@BeforeEach
	public void setup() {
		data.put("key", "value");

		when(service.hello()).thenReturn(body);
		when(service.hello(any(String.class))).thenReturn(body);
		// Mockito.doThrow(new IllegalStateException("illegal state")).when(service)
		// .hello("illegal state");

		// Mockito.doThrow(new NullPointerException("null pointer")).when(service)
		// .hello("null pointer");
		// controller = context.getBean(NoopController.class);
		// controller = new NoopController(service);
	}

	@Test
	public void test1() {
		responseString = controller.hello(data, true);
		verify(service, never()).hello();
		// or
		verify(service, times(0)).hello();
		assertThat(responseString, notNullValue());
		assertThat(responseString.isEmpty(), is(false));
		response = gson.fromJson(responseString, Map.class);
		assertThat(response, notNullValue());
		assertThat(response.containsKey("key"), is(true));

	}

	@Test
	public void test2() {
		responseString = controller.hello2(data, true);
		verify(service, never()).hello();
		// or
		verify(service, times(0)).hello();
		assertThat(responseString, notNullValue());
		assertThat(responseString.isEmpty(), is(false));
		response = gson.fromJson(responseString, Map.class);
		assertThat(response, notNullValue());
		assertThat(response.containsKey("key"), is(true));

	}

	@Test
	public void test3() {
		responseString = controller.hello(data, false);
		assertThat(responseString, notNullValue());
		verify(service).hello();
	}

}

