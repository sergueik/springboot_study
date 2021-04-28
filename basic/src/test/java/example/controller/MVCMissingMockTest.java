package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import org.junit.runner.RunWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.util.NestedServletException;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import org.hamcrest.Matcher;

import example.controller.Controller;
import example.service.ExampleService;

import example.Application;

// https://github.com/TechPrimers/test-controller-example
// https://github.com/kriscfoster/spring-boot-testing-pyramid
@RunWith(SpringJUnit4ClassRunner.class)
// @WebMvcTest
public class MVCMissingMockTest {

	final static String route = "/basic";
	final static String body = "Hello basic";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;

	// @InjectMocks
	// private Application application;

	// @Mock
	// private ExampleService mockService;

	@InjectMocks
	private Controller controller;
	private static Matcher<String> matcher;

	@Before
	public void beforeTest() throws Exception {
		matcher = containsString("mock: " + body);
		// when(mockService.hello()).thenReturn("mock: " + body);
		mvc = MockMvcBuilders.standaloneSetup(controller).build();

	}

	@Test(expected = NestedServletException.class)
	public void test1() throws Exception {
		mvc.perform(get(route)).andExpect(status().isOk());
	}

	@Test(expected = NullPointerException.class)
	public void test2() throws Throwable {
		try {
			mvc.perform(get(route)).andExpect(status().isOk());
		} catch (NestedServletException e) {
			throw (e.getRootCause());
		}
	}

	// examine root cause and slurp the exception
	@Test
	public void test3() throws Exception {
		try {
			mvc.perform(get(route)).andExpect(status().isOk());
		} catch (NestedServletException e) {
			e.getRootCause().printStackTrace();
		}
	}

}

