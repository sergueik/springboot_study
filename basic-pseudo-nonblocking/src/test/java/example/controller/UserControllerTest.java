package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.hasItem;
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
import org.junit.jupiter.api.Disabled;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

import example.model.User;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

@WebMvcTest(UserController.class)
public class UserControllerTest {

	@Autowired
	private MockMvc mockMvc;

	private MvcResult mvcResult = null;
	private ObjectMapper objectMapper = new ObjectMapper();
	private String jsonResponse = null;
	private JsonNode jsonNode;
	private User user;
	ResponseEntity<User> responseEntity = null;
	private Object objResponse;
	private Set<String> users = new HashSet<>();
	private Long id = 0L;
	private String name = null;
	private String email = null;

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
		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();

		// jsonResponse = objResponse.toString();
		// <200 OK OK,example.model.User@5600a5da,[]>

		jsonResponse = new ObjectMapper().writeValueAsString(user);
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

		// Optional: extract all top-level keys
		Set<String> keys = JsonPath.read(jsonResponse, "$.keys()");
		assertThat(keys, not(hasItem("unexpectedField"))); // ensure unwanted field is not present
		assertThat(keys, hasItem("id"));
		assertThat(keys, hasItem("name"));
		assertThat(keys, hasItem("email"));

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

		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();

		// jsonResponse = objResponse.toString();
		// <200 OK OK,example.model.User@5600a5da,[]>

		jsonResponse = new ObjectMapper().writeValueAsString(user);
		user = objectMapper.readValue(jsonResponse, User.class);

		// Step 5: assertions
		assertThat(user, is(notNullValue()));
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), is(notNullValue()));
		assertThat(user.getEmail(), containsString("@"));

	}

	@DisplayName("GetUser Deferred Success - Strong Type w/o Jackson")
	@Test
	void test13() throws Exception {
		// Step 1: call GET /users/{id} with valid ID
		mvcResult = mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk())
				.andExpect(content().string(containsString("id"))).andExpect(content().string(containsString("name")));

		// Step 3: examine async result object
		objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));

		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();

		// jsonResponse = objResponse.toString();
		// <200 OK OK,example.model.User@5600a5da,[]>

		jsonResponse = new ObjectMapper().writeValueAsString(user);

		// Step 4: extract fields via JsonPath
		Object objId = JsonPath.read(jsonResponse, "$.id");
		
		if (objId instanceof Number) {
		    id = ((Number) objId).longValue();
		}
		name = JsonPath.read(jsonResponse, "$.name");
		email = JsonPath.read(jsonResponse, "$.email");

		// Optional: extract all top-level keys
		Set<String> keys = JsonPath.read(jsonResponse, "$.keys()");
		assertThat(keys, not(hasItem("unexpectedField"))); // ensure unwanted field is not present
		assertThat(keys, hasItem("id"));
		assertThat(keys, hasItem("name"));
		assertThat(keys, hasItem("email"));

		// Step 5: map manually to new User
		User newUser = new User();
		newUser.setId(id);
		newUser.setName(name);
		newUser.setEmail(email);

		// Step 6: assertions
		assertThat(newUser, is(notNullValue()));
		assertThat(newUser.getId(), is(1L));
		assertThat(newUser.getName(), is(notNullValue()));
		assertThat(newUser.getEmail(), containsString("@"));
	}

	private Gson gson;

	@DisplayName("Deserialize GET /users/1 via Gson")
	@Test
	void testGsonDeserialize() throws Exception {
		MvcResult mvcResult = mockMvc.perform(get("/users/1")).andExpect(request().asyncStarted()).andReturn();

		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk());
		objResponse = mvcResult.getAsyncResult();
		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();
		
		// jsonResponse = mvcResult.getAsyncResult().toString();
		jsonResponse = new ObjectMapper().writeValueAsString(user);
		
		
		gson = new Gson();

		// correct class
		user = gson.fromJson(jsonResponse, User.class);
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), is(notNullValue()));
		assertThat(user.getEmail(), containsString("@"));

		// attempt de-serialization into a mismatched class
		class Foo {
			public int bar;
		}
		try {
			Foo foo = gson.fromJson(jsonResponse, Foo.class);
		} catch (JsonSyntaxException e) {
			System.err.println("Gson failed as expected: " + e.getMessage());
			assertThat(e, is(notNullValue()));
		}
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

	@Disabled
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

	@DisplayName("GetUser Method Not Allowed for invalid verb (POST)")
	@Test
	void test20() throws Exception {
		mockMvc.perform(post("/users/1").contentType("application/json")).andExpect(status().isMethodNotAllowed());
	}

	@DisplayName("GetUser Method Not Allowed for invalid verb (POST)")
	@Test
	void test21() throws Exception {
		mockMvc.perform(post("/users")).andExpect(status().isUnsupportedMediaType());
	}
}
