package example.controller;

import static org.hamcrest.CoreMatchers.containsString;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.argThat;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MvcResult;

import example.model.User;
import example.service.UserService;

@WebMvcTest(DeferredResultUserController.class)
// Only the web layer (controllers, filters, advice)	
// must @MockBean any dependent services, repositories, etc. because they are not loaded automatically

public class DeferredResultUserControllerExceptionServiceTest {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	UserService userService;

	private User user;
	private MvcResult mvcResult = null;
	private ResponseEntity<User> responseEntity = null;
	private Object objResponse;

	private static String endpoint = "/deferred/users";

	@BeforeEach
	void setup() {
		// mock service behavior
		user = new User(1L, "Alice", "a@example.com");
		// Stub the service for the specific happy path ID first
		when(userService.getUser(1L)).thenReturn(Optional.of(user));
		// however because of the next stub which Mockito may still match
		// the service for all other IDs: throw an exception
		// when(userService.getUser(anyLong()))
		// .thenThrow(new RuntimeException("test failure"));
		when(userService.getUser(argThat(id -> !id.equals(1L)))).thenThrow(new RuntimeException("test failure"));

		when(userService.getAllUsers()).thenReturn(
				Arrays.asList(new User(1L, "Alice", "a@example.com"), new User(1L, "Bob", "b@example.com")));
		when(userService.createUser(any(User.class))).thenAnswer(invocation -> {
			User user = invocation.getArgument(0);
			user.setId(2L);
			return user;
		});
	}

	// NOTE: This test demonstrates that without handling async, MockMvc only sees
	// the immediate return of the controller method (the DeferredResult object
	// itself),
	// so the status is always 200 OK (the default for the returned DeferredResult
	// wrapper)

	@DisplayName("Method Failure immediate processing")
	@Test
	void test1() throws Exception {
		mockMvc.perform(get(endpoint + "/2")).andExpect(status().isOk());
		// NOTE:
		// test written this way always OK because DeferredResult hasn't completed yet
	}

	// NOTE: This test correctly handles the asynchronous nature of DeferredResult.
	// It waits for the background thread to finish processing before asserting the
	// final HTTP status.
	@SuppressWarnings("deprecation")
	@DisplayName("Method Failure deferred processing")
	@Test
	void test2() throws Exception {

		// Step 1: Perform the GET request and expect that async processing started
		mvcResult = mockMvc.perform(get(endpoint + "/2")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: Wait for DeferredResult to complete in the background
		// asyncDispatch() resumes the request after DeferredResult is set
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().is(HttpStatus.METHOD_FAILURE.value()));
	}

	@SuppressWarnings("unchecked")
	@DisplayName("Method Success returns user")
	@Test
	void test3() throws Exception {
		mvcResult = mockMvc.perform(get(endpoint + "/1")).andExpect(request().asyncStarted()).andReturn();
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

}
