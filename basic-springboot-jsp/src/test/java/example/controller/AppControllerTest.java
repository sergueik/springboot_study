package example.controller;

/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.forwardedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.hamcrest.Matcher;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.ExampleApplication;

@WebMvcTest
public class AppControllerTest {

	final static String route = "/model";
	// final static String content = "";
	final static String forwardedUrl = "hello";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;
	private static Matcher<String> matcher;

	@InjectMocks
	private ExampleApplication application;

	@Autowired
	private AppController controller;

	@BeforeEach
	public void beforeTest() throws Exception {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(get(route));
	}

	// examine HTTP status
	// @Disabled("this test is impementation-sensitive: when HelloControler and
	// ModelController aremerged into AppController the test passes. When
	// separated the test fails")
	@Test
	public void test1() throws Exception {
		// Confirm HTTP status is not 30x
		Assertions.assertThrows(AssertionError.class, () -> {
			resultActions.andExpect(status().is3xxRedirection());
		});
		resultActions.andExpect(status().isOk());
	}

	// examine redirect URL
	// @Disabled("this test is impementation-sensitive: when HelloControler and
	// ModelController aremerged into AppController the test passes. When
	// separated the test fails")
	@Test
	public void test2() throws Exception {
		resultActions.andExpect(forwardedUrl(forwardedUrl));
	}

	// examine body - non-blank. To examine JSP page, see AcceptanceTest
	@Test
	public void test3() throws Exception {
		matcher = containsString("hello example");
		mvc.perform(get("/generate?name=example"))
				.andExpect(content().string(matcher));
	}

	// examine body to be blank
	@Test
	public void test4() throws Exception {
		resultActions.andExpect(content().string(is("")));
	}

	// examine response headers
	// NOTE: these expectations are Junit version sensitive
	@Test
	public void test5() throws Exception {
		mvc.perform(get("/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType("application/json"));
	}

	// examine response headers
	// @Disabled
	@Test
	public void test6() throws Exception {
		Assertions.assertThrows(AssertionError.class, () -> {
			mvc.perform(get(route).accept(MediaType.APPLICATION_JSON))
					.andExpect(status().isOk())
					.andExpect(content().contentType("application/json"));
		});
		resultActions.andExpect(status().isOk());
	}

}
