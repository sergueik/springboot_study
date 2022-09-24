package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import com.google.gson.Gson;

import example.controller.ExampleController.Data;
import example.service.ExampleService;
// see also:
// https://github.com/kriscfoster/spring-boot-testing-pyramid

@WebMvcTest
public class MVCIntegrationPostSetTest {

	@Autowired
	private MockMvc mvc;
	@MockBean
	// needed to prevent the following exception:
	// java.lang.IllegalStateException: Failed to load ApplicationContext
	// Caused by: org.springframework.beans.factory.UnsatisfiedDependencyException:
	// Error creating bean with name 'exampleController' defined in file
	// [../ExampleController.class]:
	// Unsatisfied dependency expressed through constructor parameter 0; nested
	// exception is org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'example.service.ExampleService' available:
	// expected at least 1 bean which qualifies as autowire candidate. Dependency
	// annotations: {}
	// Caused by: org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'example.service.ExampleService' available:
	// expected at least 1 bean which qualifies as autowire candidate. Dependency
	// annotations: {}
	private ExampleService mockService;

	private final static String fragment = "\"name\":\"two\"";
	private final static String charset = "UTF-8";
	private final static Data data = new Data("data");
	private ResultActions resultActions;
	private static final Gson gson = new Gson();
	private static final String route = "/basic/post/set";
	private static final Set<String> inputs = new HashSet<>();
	static {
		inputs.add("one");
		inputs.add("two");
	}
	final String payload = gson.toJson(inputs);

	@Test
	void test1() throws Exception {
		resultActions = mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).content(payload));

		resultActions.andDo(print()).andExpect(status().isOk()).andExpect(content().string(containsString(fragment)));
	}

	// examine HTTP status
	// 415 Unsupported Media Type when not specified
	@Test
	public void test2() throws Exception {
		resultActions = mvc.perform(post(route).content(payload));
		resultActions.andExpect(status().is(415));
	}

	// examine HTTP status
	// 400 Bad Request when backend expects json array while caller
	// provides plain text
	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).content("bad data"));
		resultActions.andExpect(status().isBadRequest());
	}

	// examine HTTP status
	// 400 Bad Request when backend expects json array while caller
	// provides plain text
	@Test
	public void test4() throws Exception {
		resultActions = mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).content("{\"bad\": \"data\"}"));
		resultActions.andExpect(status().isBadRequest());
	}

}
