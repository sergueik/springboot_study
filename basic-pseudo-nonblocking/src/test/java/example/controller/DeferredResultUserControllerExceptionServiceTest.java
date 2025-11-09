package example.controller;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.is;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.junit.jupiter.api.Assertions.assertThrows;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import javax.swing.text.html.HTMLDocument.Iterator;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

import example.model.User;
import example.service.UserService;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

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

	private static String endpoint = "/deferred/users";

	/*
	@BeforeEach
	void badSetup() {
		// mock service behavior
		user = new User(1L, "Alice", "a@example.com");
		// Stub the service for the specific happy path ID first
		when(userService.getUser(1L)).thenReturn(Optional.of(user));
		// however because of the next stub which Mockito may still match
		// the service for all other IDs: throw an exception
		when(userService.getUser(anyLong())).thenThrow(new RuntimeException("test failure"));
	}
	 */
	@BeforeEach
	void setup() {
		user = new User(1L, "Alice", "a@example.com");

		when(userService.getUser(anyLong())).thenAnswer(invocation -> {
			Long id = invocation.getArgument(0);
			if (id == 1L) {
				return Optional.of(user); // happy path
			} else {
				throw new RuntimeException("test failure"); // exception path
			}
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
		MvcResult mvcResult = mockMvc.perform(get(endpoint + "/2")).andExpect(request().asyncStarted()).andReturn();

		// Step 2: Wait for DeferredResult to complete in the background
		// asyncDispatch() resumes the request after DeferredResult is set
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().is(HttpStatus.METHOD_FAILURE.value()));
	}

	@DisplayName("Method Success returns user")
	@Test
	void test3() throws Exception {
		MvcResult mvcResult = mockMvc.perform(get(endpoint + "/1")).andExpect(request().asyncStarted()).andReturn();
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk());
	}

}
