package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.controller.ExampleController;

@SpringBootTest
@SpringBootConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = example.controller.ExampleController.class)
@TestPropertySource(properties = { "message=test message from annotation" })
public class SecondTest {

	@Autowired
	private ExampleController controller;
	private static MockMvc mvc;
	private String payload = "Value Message=test message from annotation";

	// examine HTTP status
	@Test
	public void test() throws Exception {
		final String route = "/employee";
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		assertThat(mvc, notNullValue());
		mvc.perform(get(route)).andExpect(status().isOk()).andExpect(content().string(containsString(payload)));
	}
}
