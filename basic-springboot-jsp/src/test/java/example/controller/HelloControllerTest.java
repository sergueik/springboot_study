package example.controller;

/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOError;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.forwardedUrl;

import org.hamcrest.Matcher;

import example.ExampleApplication;
// https://github.com/TechPrimers/test-controller-example
// https://github.com/kriscfoster/spring-boot-testing-pyramid
import example.controller.HelloController;

// @RunWith(SpringJUnit4ClassRunner.class)
@WebMvcTest
public class HelloControllerTest {

	final static String route = "/model";
	final static String content = "";
	final static String forwardedUrl = "hello";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;
	private static Matcher<String> matcher;

	@InjectMocks
	private ExampleApplication application;

	@Autowired
	private HelloController controller;

	@BeforeEach
	public void beforeTest() throws Exception {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(get(route));
	}

	// this test is sensitive to impementation:
	// when HelloControler and ModelController are in the same class
	// the test statusTest() and forwardUrlTest() are passing
	// examine HTTP status
	@Disabled
	@Test
	public void statusTest() throws Exception {
		// Confirm HTTP status is not 30x
		Assertions.assertThrows(AssertionError.class, () -> {
			resultActions.andExpect(status().is3xxRedirection());
		});
		resultActions.andExpect(status().isOk());
	}

	// examine redirect URL
	@Disabled
	@Test
	public void forwardUrlTest() throws Exception {
		resultActions.andExpect(forwardedUrl(forwardedUrl));
	}

	// examine body - non-blank. To examine JSP page, see AcceptanceTest
	@Test
	public void bodyTextTest() throws Exception {
		matcher = containsString("hello example");
		mvc.perform(get("/generate?name=example"))
				.andExpect(content().string(matcher));
	}

	// examine body to be blank
	@Test
	public void noBodyTest() throws Exception {
		resultActions.andExpect(content().string(is("")));
	}

	// examine response header
	@Disabled
	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		// mvc.perform(get(route ).accept(MediaType.TEXT_PLAIN)).andExpect(
		// content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}
}
