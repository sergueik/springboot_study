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
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;

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

	@DisplayName("thymeleaf rendered body test")
	@Test
	public void test4() throws Exception {
		resultActions.andExpect(
				content().string(containsString("<title>Protractor practice website - Banking App</title>")));
	}

	@DisplayName("canary for missing dialect processing")
	@Test
	public void test5() throws Exception {
		resultActions.andExpect(content().string(not(containsString("layout:decorate=\"~{layouts/layout}"))));
	}

	@DisplayName("body test")
	@Test
	public void test6() throws Exception {
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
		return Stream.of(Arguments.of("/js/account.service.js"), Arguments.of("/js/accountViewController.js"),
				Arguments.of("/js/addCustomerController.js"), Arguments.of("/js/app.js"),
				Arguments.of("/js/bodyController.js"), Arguments.of("/js/config.js"),
				Arguments.of("/js/customer.data.js"), Arguments.of("/js/customerViewController.js"),
				Arguments.of("/js/date.search.filter.js"), Arguments.of("/js/depositController.js"),
				Arguments.of("/js/listCustomerController.js"), Arguments.of("/js/mainController.js"),
				Arguments.of("/js/managerViewController.js"), Arguments.of("/js/mockDataLoadService.js"),
				Arguments.of("/js/openAccountController.js"), Arguments.of("/js/optionsController.js"),
				Arguments.of("/js/script.js"), Arguments.of("/js/transaction.service.js"),
				Arguments.of("/js/transactionSummaryController.js"), Arguments.of("/js/user.js"),
				Arguments.of("/js/user.service.js"), Arguments.of("/js/withdrawlController.js"),
				Arguments.of("/css/style.css"), Arguments.of("/css/frontend.min.css"),
				Arguments.of("/webjars/jquery/3.2.1/jquery.min.js"),
				Arguments.of("/webjars/angularjs/1.5.8/angular.js"),
				Arguments.of("/webjars/bootstrap/5.0.0/css/bootstrap.min.css"),
				Arguments.of("/webjars/bootstrap/5.0.0/js/bootstrap.min.js"),
				Arguments.of("/webjars/angular-ui-router/1.0.30/angular-ui-router.min.js"));
	}

	@DisplayName("dependencies")
	@ParameterizedTest
	@MethodSource("samples")
	void test1(final String resource) throws Exception {
		resultActions = mvc.perform(get(resource).accept(MediaType.TEXT_PLAIN));
		resultActions.andExpect(status().isOk());
	}

}
