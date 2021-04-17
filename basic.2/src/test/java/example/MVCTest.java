package example;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.CoreMatchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.hamcrest.Matcher;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

public class MVCTest {

	final static String route = "/basic";
	final static String body = "Hello basic";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;

	private static MockMvc mvc;

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(new ExampleApplication()).build();
	}

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(get(route));
	}

	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	@Test
	public void bodyTest() throws Exception {
		Matcher<String> matcher = containsString(body);
		resultActions.andExpect(content().string(matcher));
	}

	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)).andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}
}