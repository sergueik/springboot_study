package example.controller;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.InputStream;
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

import example.component.ExplicitPropertiesParser;
import example.integration.DummyController;

@WebMvcTest
public class ControllerDummyWorkerTest {

	final static String route = "/dummy";
	final static String body = "Hello dummy: other value";
	private ResultActions resultActions;
	private final String propertiesFileName = "application.properties";

	@Before
	public void beforeTest() throws Exception {
		// initialize real stuff
		final Properties properties = new Properties();
		final InputStream stream = ExplicitPropertiesParser.class.getClassLoader().getResourceAsStream(propertiesFileName);
		properties.load(stream);
		final DummyController worker = new DummyController(properties);

		final MockMvc mvc = MockMvcBuilders.standaloneSetup(worker).build();
		resultActions = mvc.perform(get(route));
	}

	@Test
	public void test() throws Exception {
		resultActions.andExpect(status().isOk()).andExpect(content().string(containsString(body)));
	}
}
