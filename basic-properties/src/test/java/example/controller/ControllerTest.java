package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import static org.hamcrest.Matchers.containsString;
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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Properties;

import example.resource.Worker;
import example.Launcher;

// NOTE: @Runwith annotation with real classes crashes the JVM
// @RunWith(SpringJUnit4ClassRunner.class)
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

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		resultActions.andExpect(content().string(containsString(body)));
	}

}
