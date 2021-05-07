package example.controller;

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

import example.controller.Controller;
import example.service.ExampleService;
import example.ExampleApplication;

// NOTE: @Runwith annotation with real classes crashes the JVM
// @RunWith(SpringJUnit4ClassRunner.class)
@WebMvcTest
public class MVCRealTest {

	final static String route = "/basic";
	final static String body = "Hello basic";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static ExampleApplication application = new ExampleApplication();
	private static ExampleService service = new ExampleService();
	private static Controller controller = new Controller(service);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
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

	// examine value
	@Test
	public void jsonTest() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.name", is(body)));
	}

	// count nodes
	@Test
	public void jsonTest2() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.*", hasSize(1)));
	}

	// examine response header
	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		charset = "ISO-8859-1";
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)).andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
		charset = "UTF-8";
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(
						String.format("application/json;charset=%s", charset)));
	}
}
