package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.forwardedUrl;

import org.hamcrest.Matcher;

import example.ExampleApplication;
// https://github.com/TechPrimers/test-controller-example
// https://github.com/kriscfoster/spring-boot-testing-pyramid
import example.HelloController;

// @RunWith(SpringJUnit4ClassRunner.class)
@WebMvcTest
public class ServiceModelTest {

	final static String route = "/";
	final static String content = "";
	final static String forwardedUrl = "hello";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;
	private static Matcher<String> matcher;

	@InjectMocks
	private ExampleApplication application;

	@Autowired
	private HelloController controller;

	@BeforeEach
	public void beforeTest() throws Exception {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(get(route));
	}

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		matcher = is(content);
		resultActions.andExpect(content().string(matcher));
	}

	@Test
	public void forwardUrlTest() throws Exception {
		resultActions.andExpect(forwardedUrl(forwardedUrl));
	}

	// examine response header
	@Disabled
	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		// mvc.perform(get(route ).accept(MediaType.TEXT_PLAIN)).andExpect(
		// content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}
}
