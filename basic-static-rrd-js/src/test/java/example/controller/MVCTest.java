package example.controller;

import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

import static org.hamcrest.CoreMatchers.containsString;

import example.Launcher;
import example.controller.ExampleController;

@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = ExampleController.class)
public class MVCTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private final static String viewName = "home";

	// NOTE: unused. Keep for future
	// NOTE: "application" is a reserved variable name
	@Value("${application}")
	private String variable;

	// System.getProperty("application");

	@BeforeEach
	public void beforeAll() throws Exception {
		resultActions = mvc.perform(get("/").accept(MediaType.TEXT_HTML));
	}

	@Test
	public void errorTest() throws Exception {
		resultActions.andExpect(model().hasNoErrors());
	}

	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	final static String body = "<html/>";

	// assert the response body content with a Hamcrest Matcher
	@Test
	public void bodyContainsTextTest() throws Exception {

		resultActions.andExpect(
				content().string(containsString("using the Flot libraries")));
	}

	@Test
	public void contentTypeTest() throws Exception {
		resultActions.andExpect(
				content().contentType(String.format("text/html;charset=%s", charset)));
	}

	@Test
	public void veiewNameTest() throws Exception {
		resultActions.andExpect(view().name(viewName));
	}

	@Disabled
	@Test
	public void verifiesHomePageLoads() throws Exception {

		Arrays.asList("variable", "hostname", "now").forEach(a -> {
			try {
				resultActions.andExpect(model().attributeExists(a));
			} catch (Exception e) {
				// for missing attributes, the
				// java.lang.AssertionError will be thrown:
				// Model attribute 'then' does not exist
				return;
			}
		});
	}

}
