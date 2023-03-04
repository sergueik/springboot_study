package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;

import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@WebMvcTest
public class ClassPathTest {
	final static String route = "/basic/classpath";
	private static MockMvc mvc;
	private static Controller controller = new Controller();
	private ResultActions resultActions;

	@BeforeClass
	public static void setUp() throws IOException {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	// examine response status and body
	@Test
	public void test1() throws Exception {
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString("target\\test-classes")));

	}

	@Test
	public void test2() throws Exception {
		resultActions = mvc.perform(get(route + "?" + "file" + "=" + "test.txt"));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString("target\\classes")));

	}

	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(get(route + "?" + "file" + "=" + "dummy.txt"));
		resultActions.andExpect(status().isBadRequest());
		resultActions.andExpect(content().string(""));

	}
}
