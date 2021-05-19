package example.controller;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.controller.Worker;

@WebMvcTest
public class ControllerTest {

	final static String route = "/worker";
	final static String body = "Hello";
	private ResultActions resultActions;

	@Before
	public void beforeTest() throws Exception {
		// initialize real stuff
		final Properties properties = new Properties();
		final Worker worker = new Worker(properties);
		worker.setTest(true);
		final MockMvc mvc = MockMvcBuilders.standaloneSetup(worker).build();
		resultActions = mvc.perform(get(route));
	}

	@Test
	public void test() throws Exception {
		resultActions.andExpect(status().isOk()).andExpect(content().string(containsString(body)));
	}
}
