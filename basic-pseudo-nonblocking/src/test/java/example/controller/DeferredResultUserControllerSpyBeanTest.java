package example.controller;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
// import org.junit.jupiter.api.Disabled;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import example.model.User;
import example.service.UserService;

import com.google.gson.Gson;

import static org.mockito.Mockito.doThrow;

@WebMvcTest(DeferredResultUserController.class)
//Only the web layer (controllers, filters, advice)	
//must @SpyBean to make Spring inject the real UserService but wrap it in a Mockito spy
@AutoConfigureMockMvc
public class DeferredResultUserControllerSpyBeanTest {

	@Autowired
	private MockMvc mockMvc;

	// inject the real service into Spring context but allow selective stubbing
	@SpyBean
	private UserService userService;
	
	private static String endpoint = "/deferred/users";

	private MvcResult mvcResult = null;
	private String jsonResponse = null;
	private User user;
	private ResponseEntity<User> responseEntity = null;
	private Object objResponse;

	@BeforeEach
	void setup() {
		// selectively stub behavior of the service		
		doThrow(new RuntimeException("test failure")).when(userService).getUser(999L);
	}

	@SuppressWarnings("unchecked")
	@DisplayName("Method Success returns user")
	@Test
	void test1() throws Exception {
		// all IDs other than 999 use the real service behavior (happy path)
		mvcResult = mockMvc.perform(get(endpoint + "/1")).andExpect(request().asyncStarted()).andReturn();
		// Step 2: async dispatch collects the final response
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk());
		// Step 3: get async result
		objResponse = mvcResult.getAsyncResult();

		assertThat(objResponse, notNullValue());

		// Assert: examine async result object
		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();
		assertThat(user, notNullValue());
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), notNullValue());
		assertThat(user.getEmail(), containsString("@"));

	}

	@DisplayName("GET /deferred/users invalid ID bad request")
	@Test
	void test2() throws Exception {
		// Step 1: call GET /users/{id} with invalid ID
		mvcResult = mockMvc.perform(get(endpoint + "/0")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch collect failure status
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isBadRequest());
	}

	@DisplayName("GET /deferred/users service exception")
	@Test
	void test3() throws Exception {
		// Step 1: call GET /users/{id} with special ID
		// which simulates a service exception to trigger METHOD_FAILURE path
		mvcResult = mockMvc.perform(get(endpoint + "/999")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: async dispatch collect failure status
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isMethodFailure());
	}
}
