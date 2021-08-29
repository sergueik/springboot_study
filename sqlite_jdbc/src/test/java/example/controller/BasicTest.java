package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.hamcrest.Matcher;
import static org.hamcrest.Matchers.containsString;

import org.mockito.Mock;
import org.mockito.InjectMocks;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.Application;
import example.entity.Result;
import example.service.BaseService;

// https://github.com/TechPrimers/test-controller-example
// https://github.com/kriscfoster/spring-boot-testing-pyramid

@RunWith(SpringJUnit4ClassRunner.class)
// @WebMvcTest
public class BasicTest {

	final static String route = "/student/findAllStudent";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;

	@InjectMocks
	private Application application;

	@InjectMocks
	private Controller controller;

	@Mock
	private BaseService mockService;

	private static Matcher<String> matcher;

	@Before
	public void beforeTest() throws Exception {
		matcher = containsString("{\"status\":0,\"data\":null}");
		when(mockService.findAllStudent()).thenReturn(new Result());
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(post(route));
	}

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		resultActions.andExpect(content().string(matcher));
		mvc.perform(post(route)).andExpect(content().string(matcher));
	}

	// examine backend call
	@Test
	public void subjectMethodTest() throws Exception {
		verify(mockService).findAllStudent();
	}

	// examine response header
	@Ignore
	// TODO: set up controller to produce desired content type headers
	// NOTE: these expectations are Junit version sensitive
	@Test
	public void contentTypeTest() throws Exception {
		// mvc.perform(post(route ).accept(MediaType.TEXT_PLAIN)).andExpect(
		// content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(post(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}
}