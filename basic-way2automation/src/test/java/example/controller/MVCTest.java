package example.controller;

import java.util.Arrays;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
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

import example.controller.HomeController;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = HomeController.class)
public class MVCTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private final static String viewName = "home";

	// NOTE: "application" is a reserved variable name
	@Value("${application}")
	private String variable;

	// System.getProperty("application");

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(get("/" + variable).accept(MediaType.TEXT_HTML));
	}

	@Test
	public void errorTest() throws Exception {
		resultActions.andExpect(model().hasNoErrors());
	}

	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	@DisplayName("body test")
	@Test
	public void test4() throws Exception {
		resultActions.andExpect(
				content().string(containsString("<title>Protractor practice website - Banking App</title>")));
	}

	@DisplayName("body test")
	@Test
	public void test5() throws Exception {
		resultActions.andExpect(content().contentType(String.format("text/html;charset=%s", charset)));
	}

	@DisplayName("only relevant for thymeleaf")
	@Test
	public void test2() throws Exception {

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

	@DisplayName("only relevant for thymeleaf")
	@Test
	public void test3() throws Exception {
		resultActions.andExpect(view().name(viewName));
	}

	static Stream<Arguments> samples() {
		return Stream.of(Arguments.of("account.service.js"), Arguments.of("accountViewController.js"),
				Arguments.of("addCustomerController.js"), Arguments.of("app.js"), Arguments.of("bodyController.js"),
				Arguments.of("config.js"), Arguments.of("customer.data.js"), Arguments.of("customerViewController.js"),
				Arguments.of("date.search.filter.js"), Arguments.of("depositController.js"),
				Arguments.of("listCustomerController.js"), Arguments.of("mainController.js"),
				Arguments.of("managerViewController.js"), Arguments.of("mockDataLoadService.js"),
				Arguments.of("openAccountController.js"), Arguments.of("optionsController.js"),
				Arguments.of("script.js"), Arguments.of("transaction.service.js"),
				Arguments.of("transactionSummaryController.js"), Arguments.of("user.js"),
				Arguments.of("user.service.js"), Arguments.of("withdrawlController.js"));
	}

	@DisplayName("dependencies")
	@ParameterizedTest
	@MethodSource("samples")
	void test1(String script) throws Exception {
		resultActions = mvc.perform(get("/js/" + script).accept(MediaType.TEXT_PLAIN));
		resultActions.andExpect(status().isOk());
	}

}
