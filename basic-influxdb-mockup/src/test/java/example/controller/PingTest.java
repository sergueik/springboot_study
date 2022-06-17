package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.head;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

@WebMvcTest
public class PingTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	private final static String route = "/ping";
	private final static String param = "X-Influxdb-version";
	private final static String value = "OSS";

	@BeforeEach
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(head(route));
	}

	@Test
	void test() throws Exception {
		resultActions.andDo(print()).andExpect(status().isNoContent())
				.andExpect(header().string(param, value));
	}
}
