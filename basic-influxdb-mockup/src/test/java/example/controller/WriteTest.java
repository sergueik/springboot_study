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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@WebMvcTest
public class WriteTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	// TODO: query params
	private final static String database = "example";
	private final static String route = "/write";
	private final static String payload = "testing,host=lenovo120S,env=UAT,appid=FOO,operation=write value1=42.0 value2=10.0 1655496818";
	private final static String param = "result";
	private final static String value = "measurement=testing\ntag_set=,host=lenovo120S,env=UAT,appid=FOO,operation=write\nfield_set=value1=42.0 value2=10.0\ntimestamp=1655496818\n";

	@BeforeEach
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(post(route).queryParam("db", database)
				.contentType("application/x-www-form-urlencoded").content(payload));
	}

	@Test
	void test() throws Exception {
		resultActions.andDo(print()).andExpect(status().isNoContent())
				.andExpect(header().string(param, value));
	}
}
