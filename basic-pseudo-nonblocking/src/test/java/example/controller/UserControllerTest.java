package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */


import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Callable;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

@WebMvcTest(UserController.class)
public class UserControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@BeforeEach
	void setup() {
		// optional: mock services if needed
	}

	@DisplayName("GetUser Deferred Success")
	@Test
	void test1() throws Exception {
		// Step 1: call GET /users/{id} with valid ID
		MvcResult mvcResult = mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch(mvcResult))
				.andExpect(status().isOk()).andExpect(content().string(containsString("id")))
				.andExpect(content().string(containsString("name")));

		// Optional: inspect async result object
		Object resultObj = mvcResult.getAsyncResult();
		assertThat(resultObj, is(notNullValue()));
	}

	@DisplayName("GetUserDeferredInvalidId")

	@Test
	void test2() throws Exception {
		// simulate negative or zero ID
		MvcResult mvcResult = mockMvc.perform(get("/users/0")).andExpect(request().asyncStarted()).andReturn();

		mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch(mvcResult))
				.andExpect(status().is4xxClientError()).andExpect(content().string(containsString("invalid id")));

		Object resultObj = mvcResult.getAsyncResult();
		assertThat(resultObj, is(notNullValue()));
	}

	@DisplayName("testPostUserDeferredSuccess")
	@Test
	void test3() throws Exception {
		// Step 1: POST /users with valid payload
		String jsonPayload = "{\"name\":\"John Doe\",\"email\":\"john@example.com\"}";

		MvcResult mvcResult = mockMvc.perform(post("/users").contentType("application/json").content(jsonPayload))
				.andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch(mvcResult))
				.andExpect(status().isCreated()).andExpect(content().string(containsString("id")))
				.andExpect(content().string(containsString("John Doe")));

		Object resultObj = mvcResult.getAsyncResult();
		assertThat(resultObj, is(notNullValue()));
	}

	@DisplayName("testGetAllUsersDeferredCollection")
	@Test
	void test4() throws Exception {
		// GET /users (returns collection)
		MvcResult mvcResult = mockMvc.perform(get("/users")).andExpect(request().asyncStarted()).andReturn();

		mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch(mvcResult))
				.andExpect(status().isOk()).andExpect(content().string(containsString("Alice")))
				.andExpect(content().string(containsString("Bob")))
				.andExpect(content().string(containsString("Charlie")));

		Object resultObj = mvcResult.getAsyncResult();
		assertThat(resultObj, is(notNullValue()));

		Set<String> users = new HashSet<>(Arrays.asList((String[]) resultObj)); // cast to actual type
		//
		assertThat(users, containsInAnyOrder("Alice", "Bob", "Charlie"));
	}
}
