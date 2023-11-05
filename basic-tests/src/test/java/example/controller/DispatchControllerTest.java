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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

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

	// https://docs.oracle.com/javaee/6/api/javax/servlet/http/HttpServletResponse.html
	// https://docs.oracle.com/javaee%2F6%2Fapi%2F%2F/javax/servlet/ServletResponse.html
	// see also:
	// https://stackoverflow.com/questions/51739333/how-to-create-httpservletresponse-for-unit-tests-in-spring
	@MockBean
	private HttpServletResponse response;

	private String responseString;

	@BeforeEach
	public void setup() {

		when(service.hello()).thenReturn(body);
		when(service.hello(any(String.class))).thenReturn(body);
		Mockito.doThrow(new IllegalStateException("illegal state")).when(service)
				.hello("illegal state");

		Mockito.doThrow(new NullPointerException("null pointer")).when(service)
				.hello("null pointer");
		controller = context.getBean(DispatchController.class);
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
		assertThat(responseEntity.getBody(), nullValue());
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

	@Test
	public void test4() {
		responseString = controller.callService(name, response);
		assertThat(responseString, notNullValue());
		assertThat(response, notNullValue());
		verify(response).setStatus(HttpStatus.OK.value());
		assertThat(responseString.isEmpty(), is(false));

	}

	@Test
	public void test5() {
		responseString = controller.callService("illegal state", response);
		assertThat(responseString, nullValue());
		verify(response).setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());

	}

	@Test
	public void test6() {
		responseString = controller.callService("null pointer", response);
		assertThat(responseString, nullValue());
		verify(response).setStatus(HttpStatus.METHOD_NOT_ALLOWED.value());
	}
}
