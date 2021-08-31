package example;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.Test;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.ExampleController;

@WebMvcTest
public class ExampleControllerTest {

	private static ExampleController controller = new ExampleController();
	private static MockMvc mvc;

	// examine HTTP status
	@Test
	public void test() throws Exception {
		final String route = "/employee";
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		assertThat(mvc, notNullValue());
		mvc.perform(get(route)).andExpect(status().isOk());
	}
}
