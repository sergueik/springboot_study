package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.text.IsEmptyString.isEmptyString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.cookie;

import example.controller.Controller;

@WebMvcTest
public class CookieTest {

	static String route = "/hello";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;
	private final static String default_name = "default_name";

	private static Controller controller = new Controller();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
	}

	@Test
	public void test1() throws Exception {
		route = "/basic/init";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		resultActions.andExpect(content().string(""));
		resultActions.andExpect(cookie().exists(default_name));

	}

	@Test
	public void test2() throws Exception {
		route = "/basic/clear";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		resultActions.andExpect(content().string(""));
		resultActions.andExpect(cookie().exists(default_name));
		resultActions.andExpect(cookie().value(default_name, isEmptyString()));

	}
}
