package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import org.mockito.ArgumentMatcher;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.google.gson.Gson;

import example.service.ExampleService;
import example.controller.ExampleController.Data;
// see also:
// https://github.com/kriscfoster/spring-boot-testing-pyramid

@WebMvcTest
public class IntegrationTest {

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;

	private final static String route = "/basic";
	// NOTE: exercising property file override
	private final static String body = "Hello test data";
	private final static String charset = "UTF-8";
	private final static Data data = new Data("data");
	private ResultActions resultActions;
	private static final Gson gson = new Gson();
	private final static MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();
	private final static String payload = gson.toJson(data);
	private final static String responsePayload = gson.toJson(data);

	@BeforeEach
	public void beforeTest() throws Exception {
		when(mockService.hello()).thenReturn(body);
		resultActions = mvc.perform(get(route));
	}

	// TODO: check the quotes issue
	// it is related to "spring.http.converters.preferred-json-mapper=gson" and
	// "spring.gson.disable-html-escaping=true" in "application.properties"
	@Test
	void alltTest() throws Exception {
		// resultActions.andDo(print()).andExpect(status().isOk())
		// .andExpect(content().string(equalTo(String.format("\"%s\"", body))));
		resultActions.andDo(print()).andExpect(status().isOk())
				.andExpect(content().string(equalTo(body)));
		verify(mockService).hello();
	}

	// examine backend call
	@Test
	public void subjectMethodTest() throws Exception {
		verify(mockService).hello();
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

	// examine value
	@Test
	public void jsonTest() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.name", is(body)));
	}

	// count nodes
	@Test
	public void jsonTest2() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.*", hasSize(1)));
	}

	// examine response header
	// NOTE: these expectations are Junit version sensitive
	@Test
	public void contentTypeTest() throws Exception {
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)).andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}

	@Disabled
	// TODO:
	// Expected: a string containing "{\"name\":\"data\"}" but: was ""
	@Test
	void postJSONTest() throws Exception {

		mvc.perform(post(route + "/post/json").contentType("application/json")
				.content(payload)).andDo(print()).andExpect(status().isOk())
				.andExpect(content().string(containsString(payload)))
				.andExpect(jsonPath("$.name", is(data.getName())));

		// see also:
		// https://www.baeldung.com/mockito-argument-matchers
		verify(mockService).handleData(any(ExampleController.Data.class));

		// see also
		// https://www.codota.com/code/java/methods/org.mockito.Mockito/argThat

		// Wanted but not invoked:
		// Actual invocations have different arguments:
		/*
		 * verify(mockService).handleData(argThat(new
		 * ArgumentMatcher<ExampleController.Data>() {
		 * 
		 * @Override public boolean matches(ExampleController.Data argument) {
		 * System.err.println("Simple name: " + argument.getClass().getSimpleName());
		 * return argument.getClass().getSimpleName().equals("ExampleController.Data");
		 * } })); // TODO: lambda verify(mockService) .handleData(argThat(argument ->
		 * argument.getClass().getSimpleName().equals("ExampleController.Data")));
		 */
	}

	@Test
	void postFormTest() throws Exception {
		// set up mock to respond with data to any argument of the signature
		when(mockService.handleData(any(ExampleController.Data.class)))
				.thenReturn(data);
		// TODO: format argument Object as @RequestBody directly
		params.add("name", data.getName());
		mvc.perform(post(route + "/post/form")
				.contentType("application/x-www-form-urlencoded").params(params)
				.accept(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isOk())
				.andExpect(content().string(containsString(responsePayload.toString())))
				.andExpect(jsonPath("$.name", is(data.getName())));
		verify(mockService).handleData(any(ExampleController.Data.class));

	}

	@Test
	void gettFormTest() throws Exception {
		params.add("name", data.getName());
		mvc.perform(get(route + "/post/form")
				.contentType("application/x-www-form-urlencoded").params(params)
				.accept(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isMethodNotAllowed());
	}

	@Test
	void postEmptyParamFormTest() throws Exception {
		mvc.perform(post(route + "/post/form")
				.contentType("application/x-www-form-urlencoded")
				.params(new LinkedMultiValueMap<String, String>())
				.accept(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isBadRequest());
	}

}
