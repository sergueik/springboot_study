package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.ReadContext;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import example.model.User;

@WebMvcTest(CallableForUserController.class)
public class CallableForUserControllerTest {

	@Autowired
	private MockMvc mockMvc;
	private String endpoint = "/callable/users";
	private MvcResult mvcResult = null;

	@BeforeEach
	void setup() throws Exception {
		String jsonPayload = "{\"name\":\"Alice\", \"email\":\"a@example.com\", \"id\":1}";
		mvcResult = mockMvc.perform(post(endpoint).contentType("application/json").content(jsonPayload))
				.andExpect(request().asyncStarted()).andReturn();
		// wait for async completion.
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isCreated());
	}

	@DisplayName("GET /callable/users/1 returns non-null and valid User")
	@Test
	void test1() throws Exception {

		// Step 1: perform GET request to /callable/users/1
		MvcResult mvcResult = mockMvc.perform(get(endpoint + "/1")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: dispatch async result
		mvcResult = mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk()).andReturn();

		// Step 3: get async result object
		Object objResponse = mvcResult.getAsyncResult();
		assertThat(objResponse, is(notNullValue()));

		// Step 4: cast to HttpEntity<User>
		@SuppressWarnings("unchecked")
		HttpEntity<User> responseEntity = (HttpEntity<User>) objResponse;
		User user = responseEntity.getBody();
		assertThat(user, is(notNullValue()));

		// Step 5: serialize to JSON string for JsonPath querying
		String jsonResponse = String.format("{\"id\":%d,\"name\":\"%s\",\"email\":\"%s\"}", user.getId(),
				user.getName(), user.getEmail());

		// Step 6: use JsonPath to verify name
		ReadContext ctx = JsonPath.parse(jsonResponse);
		String name = ctx.read("$.name", String.class);
		assertThat(name, is(notNullValue()));
	}
}
