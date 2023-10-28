package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.google.gson.Gson;

import example.controller.DispatchController;
import example.service.SimpleService;

@SpringBootTest
public class DispatchControllerTest {
	@Autowired
	ApplicationContext context;
	// @Autowired
	@InjectMocks
	// NOTE: incommenting leads to massive exceptio stack and VM crash
	DispatchController controller;

	@MockBean
	SimpleService service;

	private static String name = "name";
	final static String body = "Hello";
	// NOTE: have to match signature generic modifier
	// type mismatch: cannot convert from ResponseEntity<capture#1-of ?> to
	// ResponseEntity<Object>
	// private ResponseEntity<Object> responseEntity = null;
	private ResponseEntity<String> responseEntity = null;

	@BeforeEach
	public void setup() {

		when(service.hello()).thenReturn(body);
		when(service.hello(any(String.class))).thenReturn(body);
		Mockito.doThrow(new IllegalStateException("illegal state")).when(service)
				.hello("illegal state");

		Mockito.doThrow(new NullPointerException("null pointer")).when(service)
				.hello("null pointer");		controller = context.getBean(DispatchController.class);
	}

	@Test
	public void test1() {
		responseEntity = controller.callService(name);
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String respose = responseEntity.getBody();
		assertThat(respose, notNullValue());
		assertThat(respose.isEmpty(), is(false));

	}

	@Test
	public void test2() {
		responseEntity = controller.callService("illegal state");
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getStatusCode(),
				is(HttpStatus.INTERNAL_SERVER_ERROR));

	}

	@Test
	public void test3() {
		responseEntity = controller.callService("null pointer");
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getBody(), nullValue());
		assertThat(responseEntity.getStatusCode(),
				is(HttpStatus.METHOD_NOT_ALLOWED));

	}
}
