package example.integration;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.InputStream;
import java.util.Properties;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.component.ExplicitPropertiesParser;
import example.controller.Worker;

@WebMvcTest
public class IntegrationDummyControllerTest {

	@Autowired
	private MockMvc mvc;

	private final static String route = "/dummy";
	private final static String body = "Hello dummy: other value";
	private ResultActions resultActions;
	private final String propertiesFileName = "application.properties";

	@Before
	public void beforeTest() throws Exception {
		final Properties properties = new Properties();
		final InputStream stream = ExplicitPropertiesParser.class.getClassLoader().getResourceAsStream(propertiesFileName);
		properties.load(stream);
		// String resourcePath =
		// Thread.currentThread().getContextClassLoader().getResource("").getPath();
		final DummyController worker = new DummyController(properties);
		mvc = MockMvcBuilders.standaloneSetup(worker).build();
		resultActions = mvc.perform(get(route));
	}

	@Test
	public void test() throws Exception {
		resultActions.andDo(print()).andExpect(status().isOk()).andExpect(content().string(containsString(body)));
	}

}
