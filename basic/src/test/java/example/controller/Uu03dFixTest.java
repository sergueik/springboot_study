package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Before;
import org.junit.BeforeClass;

import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.ExampleApplication;
import example.service.ExampleService;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class Uu03dFixTest {

	private static String route = "/basic/uu03d";
	private static String body;
	private ResultActions resultActions;
	private static MockMvc mvc;

	@SuppressWarnings("unused")
	private static ExampleApplication application = new ExampleApplication();
	private static ExampleService service = new ExampleService();
	private static Controller controller = new Controller(service);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void before() throws Exception {

	}

	private String charset;

	@Test
	public void test0() throws Exception {
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
	}

	@Test
	
	public void test5() throws Exception {
		charset = "UTF-8";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content().contentType(String.format("application/json;charset=%s", charset)));

	}

	@Test
	public void test1() throws Exception {
		route = "/basic/uu03d?fix=false";
		resultActions = mvc.perform(get(route));
		body = "0\\u003dRUNNING,1\\u003dRUNNING,2\\u003dRUNNING";
		resultActions.andExpect(content().string(containsString(body)));
		// there is auto conversion
		body = "0=RUNNING,1=RUNNING,2=RUNNING";
		resultActions.andExpect(jsonPath("$.data", is(body)));
	}

	@Test
	public void test2() throws Exception {
		route = "/basic/uu03d?fix=true";
		resultActions = mvc.perform(get(route));
		body = "0=RUNNING,1=RUNNING,2=RUNNING";

		resultActions.andExpect(content().string(containsString(body)));
		// there is no auto conversion
		resultActions.andExpect(jsonPath("$.data", is(body)));
	}

	@Test
	public void test3() throws Exception {
		route = "/basic/uu03draw";
		resultActions = mvc.perform(get(route));
		body = "0=RUNNING,1=RUNNING,2=RUNNING";

		resultActions.andExpect(content().string(containsString(body)));
		// there is no auto conversion
		resultActions.andExpect(jsonPath("$.data", is(body)));
	}

}
