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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;

import javax.swing.text.html.HTMLDocument.Iterator;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

import example.model.User;

@WebMvcTest(UserController.class)
public class UserControllerTest {

	@Autowired
	private MockMvc mockMvc;

	private MvcResult mvcResult = null;
	private ObjectMapper objectMapper = new ObjectMapper();
	private String jsonResponse = null;
	private JsonNode jsonNode;
	private User user;
	private Object objResponse;
	private Set<String> users = new HashSet<>();

	@BeforeEach
	void setup() {
		// optional: mock services if needed
	}

	@DisplayName("GetUser with valid ID Deferred Success")
	@Test
	void test10() throws Exception {
		// Step 1: call GET /users/{id} with valid ID
		mvcResult = mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk())
				.andExpect(content().string(containsString("id"))).andExpect(content().string(containsString("name")));

		// Step 3: get async result
		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));

		// Assert: examine async result object

		jsonResponse = objResponse.toString();
		jsonNode = objectMapper.readTree(jsonResponse);

		assertThat(jsonNode.get("id").asLong(), is(1L));
		assertThat(jsonNode.get("name").asText(), is(notNullValue()));
		assertThat(jsonNode.get("email").asText(), containsString("@"));

		// use JsonPath for path-based assertions
		String email = JsonPath.read(jsonResponse, "$.email");
		assertThat(email, containsString("@"));

		// Assert that only id, name, email exist

		// Get the field names
		java.util.Iterator<String> fieldsIterator = jsonNode.fieldNames();
		List<String> fieldNames = new ArrayList<>();
		fieldsIterator.forEachRemaining(fieldNames::add);
		assertThat(fieldNames, containsInAnyOrder("id", "name", "email"));
		assertThat(fieldNames.size(), is(3)); // exactly three fields
	}

	@DisplayName("GetUser Deferred Success")
	@Test
	void test12() throws Exception {
		// Step 1: call GET /users/{id} with valid ID
		mvcResult = mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk())
				.andExpect(content().string(containsString("id"))).andExpect(content().string(containsString("name")));

		// examine async result object
		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));
		// deserialize to model type
		jsonResponse = objResponse.toString();
		user = objectMapper.readValue(jsonResponse, User.class);

		// Step 5: assertions
		assertThat(user, is(notNullValue()));
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), is(notNullValue()));
		assertThat(user.getEmail(), containsString("@"));

	}

	/*
	 * @DisplayName("GetUser Deferred Success")
	 * 
	 * @Test
	 * 
	 * void test13() throws Exception { // Step 1: call GET /users/{id} with valid
	 * ID MvcResult mvcResult =
	 * mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).
	 * andReturn();
	 * 
	 * // Step 2: async dispatch
	 * mockMvc.perform(org.springframework.test.web.servlet.request.
	 * MockMvcRequestBuilders.asyncDispatch(mvcResult))
	 * .andExpect(status().isOk()).andExpect(content().string(containsString("id")))
	 * .andExpect(content().string(containsString("name")));
	 * 
	 * // examine async result object Object objResponse =
	 * mvcResult.getAsyncResult(); assertThat(objResponse, is(notNullValue())); //
	 * de-serialize to model type without ObjectMapper String json =
	 * objResponse.toString(); Long id = ((Number)
	 * org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath(
	 * "$.id") .match(json)).longValue(); String name = (String)
	 * org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath(
	 * "$.name") .match(json); String email = (String)
	 * org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath(
	 * "$.email") .match(json);
	 * 
	 * jsonResponse = objResponse.toString(); JsonNode node =
	 * objectMapper.readTree(jsonResponse); assertThat(node.get("id").asLong(),
	 * is(1L)); assertThat(node.get("name").asText(), is(notNullValue()));
	 * assertThat(node.get("email").asText(), containsString("@"));
	 * 
	 * // Step 3 (optional): use JsonPath for path-based assertions email =
	 * JsonPath.read(jsonResponse, "$.email"); assertThat(email,
	 * containsString("@"));
	 * 
	 * user = objectMapper.readValue(jsonResponse, User.class);
	 * 
	 * // Step 5: assertions assertThat(user, is(notNullValue()));
	 * assertThat(user.getId(), is(1L)); }
	 */

	@DisplayName("GetUserDeferredInvalidId")
	@Test
	void test2() throws Exception {
		// simulate negative or zero ID
		mvcResult = mockMvc.perform(get("/users/0")).andExpect(request().asyncStarted()).andReturn();

		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().is4xxClientError())
				.andExpect(content().string(containsString("invalid id")));

		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));
	}

	@DisplayName("testPostUserDeferredSuccess")
	@Test
	void test3() throws Exception {
		// Step 1: POST /users with valid payload
		String jsonPayload = "{\"name\":\"John Doe\", \"email\":\"john@example.com\"}";

		mvcResult = mockMvc.perform(post("/users").contentType("application/json").content(jsonPayload))
				.andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isCreated())
				.andExpect(content().string(containsString("id")))
				.andExpect(content().string(containsString("John Doe")));

		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));
	}

	@DisplayName("testGetAllUsersDeferredCollection")
	@Test
	void test4() throws Exception {
		// GET /users (returns collection)
		MvcResult mvcResult = mockMvc.perform(get("/users")).andExpect(request().asyncStarted()).andReturn();

		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk())
				.andExpect(content().string(containsString("Alice"))).andExpect(content().string(containsString("Bob")))
				.andExpect(content().string(containsString("Charlie")));

		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));

		users = new HashSet<>(Arrays.asList((String[]) objResponse)); // cast to actual type
		//
		assertThat(users, containsInAnyOrder("Alice", "Bob", "Charlie"));
	}
}
