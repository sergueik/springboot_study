package example.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@WebMvcTest
public class HealthCheckTest {

	// examine HTTP status
	@Test
	public void test() throws Exception {
		final String route = "/";
		MockMvcBuilders.standaloneSetup(new HealthCheckController()).build()
				.perform(get(route)).andExpect(status().isOk());
	}
}
