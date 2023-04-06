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

// import com.google.gson.Gson;

import example.ExampleApplication;
import example.controller.HtmlEscapeController;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class HtmlEscapeControllerTest {

	// private static final Gson gson = new Gson();
	private static String route;
	private final String body = "0=RUNNING,1=RUNNING,2=RUNNING";
	private final String body_escaped = body.replaceAll("=", "\\\\u003d");
	private ResultActions resultActions;
	private static MockMvc mvc;
	private String charset;

	@SuppressWarnings("unused")
	private static ExampleApplication application = new ExampleApplication();
	private static HtmlEscapeController controller = new HtmlEscapeController();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void before() throws Exception {

	}

	@Test
	public void test0() throws Exception {
		route = "/htmlescape/basic";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
	}

	@Test
	public void test5() throws Exception {
		route = "/htmlescape/basic";
		charset = "ISO-8859-1";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content()
				.contentType(String.format("application/json;charset=%s", charset)));

	}

	@Test
	public void test6() throws Exception {
		route = "/htmlescape/legacy";
		charset = "UTF-8";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content()
				.contentType(String.format("application/json;charset=%s", charset)));
		resultActions.andExpect(
				content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

	}

	@Test
	public void test7() throws Exception {
		route = "/htmlescape/with_charset";
		charset = "UTF-8";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content()
				.contentType(String.format("application/json;charset=%s", charset)));
		// providing the encoding does not fix the html escaping issue
		resultActions.andExpect(content().string(containsString(body_escaped)));

	}

	@Test
	public void test1() throws Exception {
		route = "/htmlescape/basic" + "?fix=false";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content().string(containsString(body_escaped)));
		// observed auto conversion
		resultActions.andExpect(jsonPath("$.data", is(body)));
		// NOTE: gson.fromJson() also auto-converts - unsure how to test
	}

	@Test
	public void test2() throws Exception {
		route = "/htmlescape/basic" + "?fix=true";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content().string(containsString(body)));
		resultActions.andExpect(jsonPath("$.data", is(body)));
	}

	@Test
	public void test3() throws Exception {
		route = "/htmlescape/legacy";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(content().string(containsString(body)));
		resultActions.andExpect(jsonPath("$.data", is(body)));
	}

}
