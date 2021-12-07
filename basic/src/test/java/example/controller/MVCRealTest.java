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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.controller.Controller;
import example.service.ExampleService;
import example.ExampleApplication;

// TODO: at commit 0d9bd8110fb26138420a0297a48049c1f49604a0
// all tests in MVCRealTest are broken
// with No mapping found for HTTP request with URI [/hello]
// and subsequent assertion errors

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
	// TODO: MVCRealTest.statusTest:59 Status expected:<200> but was:<404>
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	// TODO: Response content Expected: a string containing "Hello basic" but: was
	// ""
	public void bodyTest() throws Exception {
		resultActions.andExpect(content().string(containsString(body)));
	}

	// examine value
	// TODO: No value at JSON path "$.name", exception: json can not be null or
	// empty
	@Test
	public void jsonTest() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.name", is(body)));
	}

	// count nodes
	// TODO: No value at JSON path "$.*", exception: json can not be null or empty
	@Test
	public void jsonTest2() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.*", hasSize(1)));
	}

	// examine response header
	// TODO: Content type not set
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

	// count nodes
	// TODO: No value at JSON path "$.*", exception: json can not be null or empty
	@Test
	public void postTest() throws Exception {
		mvc.perform(post(route + "/page").param("name", new String[] { "name" }))
				.andExpect(content().string(containsString("name")));
	}

}
