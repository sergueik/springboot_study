package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.Application;
import example.service.ExampleService;

@WebMvcTest
public class TagValuesRequestTest {

	final static String route = "/tag-values";
	final static String body = "one";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;
	@SuppressWarnings("unused")
	private static Application application = new Application();
	private static ExampleService service = new ExampleService();
	private static TagController controller = new TagController(service);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {

		resultActions = mvc.perform(post(route).accept(MediaType.APPLICATION_JSON)
				.content("{}").contentType(MediaType.APPLICATION_JSON));
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

	// TODO: Applies for other API than /search
	@Ignore
	@Test
	public void jsonTest() throws Exception {
		resultActions.andExpect(jsonPath("$.name", is(body)));
	}

	// count nodes
	@Test
	public void jsonTest2() throws Exception {
		resultActions.andExpect(jsonPath("$.*", hasSize(3)));
	}

	@Test
	// TODO : confirm rejections
	// NOTE: these expectations are Junit version sensitive
	public void rejectedMethodTest() throws Exception {
		mvc.perform(get(route)).andExpect(status().isMethodNotAllowed());
	}

	// Required request body is missing:
	@Test
	public void rejectionsTest() throws Exception {
		mvc.perform(post(route).contentType(MediaType.TEXT_PLAIN))
				.andExpect(status().isUnsupportedMediaType());
		mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isBadRequest());
		mvc.perform(
				post(route).contentType(MediaType.APPLICATION_JSON).content("{}"))
				.andExpect(status().isOk());
		charset = "UTF-8";
		mvc.perform(post(route)
				.contentType(String.format("application/json;charset=%s", charset))
				.content("{\"key\": \"city\"}")).andExpect(status().isOk());
	}

	// examine response header
	@Test
	public void contentTypeTest() throws Exception {
		charset = "UTF-8";
		// NOTE: these expectations are Junit version sensitive
		mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).content("{}"))
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_VALUE));
	}

	@Test(expected = AssertionError.class)
	// application/json;charset=UTF-8
	public void contentTypeCharsetTest() throws Exception {
		mvc.perform(post(route).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

}
