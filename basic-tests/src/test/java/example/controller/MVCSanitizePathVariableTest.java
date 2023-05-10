package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsArrayWithSize.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assumptions;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/result/JsonPathResultMatchers.html
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
// requires a later version ?
// import static org.springframework.test.web.servlet.result.JsonPathResultMatchers.isArray;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import example.controller.ExampleController;
import example.service.ExampleService;
import example.Application;

@WebMvcTest
public class MVCSanitizePathVariableTest {

	static String route = "/basic/sanitize/";
	// http://www.w3schools.com/tags/ref_urlencode.ASP
	static final List<String> badFilenames = Arrays.asList(
			new String[] { "%03Ctest", "test%24", "test$$", "test!", "test&" });
	static final List<String> goodFilenames = Arrays.asList(
			new String[] { "alphanumric123", "white space is OK", "_-123", "test@" });
	static final List<String> invalidFilenames = Arrays
			.asList(new String[] { "test?" });
	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;
	private ResultActions resultActions;

	// examine HTTP status and body - missing request params
	@Test
	public void test1() throws Exception {
		for (String filename : badFilenames) {
			resultActions = mvc.perform(get(route + "/" + filename + "/data"));
			resultActions.andExpect(status().isMethodNotAllowed())
					.andExpect(content().string("invalid filename"));
		}
	}

	@Test
	public void test2() throws Exception {
		for (String filename : goodFilenames) {
			resultActions = mvc.perform(get(route + "/" + filename + "/data"));
			resultActions.andExpect(status().isMethodNotAllowed())
					.andExpect(content().string("invalid filename"));
		}
	}

	@Test
	public void test3() throws Exception {
		for (String filename : invalidFilenames) {
			resultActions = mvc.perform(get(route + "/" + filename + "/data"));
			resultActions.andExpect(status().isNotFound())
					.andExpect(content().string(""));
		}
	}

}
