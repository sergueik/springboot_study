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
public class MVCQueryParamTest {

	static String route = "/basic/queryparam";

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;
	private ResultActions resultActions;

	// examine HTTP status and body - missing request params
	@Test
	public void test1() throws Exception {

		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isMethodNotAllowed());
		resultActions.andExpect(content().string(""));
	}

	// examine HTTP status and body, custom formatting of query params
	@Test
	public void test2() throws Exception {

		resultActions = mvc.perform(get(route + "?ids=1,2,3,4,5&appids=foo,bar"));
		resultActions.andExpect(status().isOk());
		resultActions.andExpect(
				content().string(containsString("appids: foo,bar ids: 1,2,3,4,5")));
	}

}
