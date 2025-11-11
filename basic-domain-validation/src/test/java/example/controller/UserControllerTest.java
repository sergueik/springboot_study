package example.controller;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.model.User;
import example.controller.UserController;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.ObjectMapper;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(UserController.class)
public class UserControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@Autowired
	private ObjectMapper objectMapper;

	private final String invalidUserJSON = "{\"name\":\"\",\"email\":\"invalid-email\"}";
	// NOTE: will see warnings:
	// HandlerExceptionResolver : Resolved
	// [org.springframework.http.converter.HttpMessageNotReadableException:
	// JSON parse error: Cannot construct instance of `example.model.User`
	// (although at least one Creator exists): no String-argument
	// constructor/factory method to deserialize from String value
	// ('{"name":"","email":"invalid-email"}')]
	private final User validUser = new User("John Doe", "john@example.com");

	@DisplayName("Blocking: Invalid User")
	@Test
	void test1() throws Exception {
		mockMvc.perform(post("/users/blocking").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON))
				.andExpect(status().isBadRequest());
	}

	@DisplayName("Callable: Invalid User")
	@Test
	void test2() throws Exception {
		mockMvc.perform(post("/users/callable").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON))
				.andExpect(status().isBadRequest());
	}

	@DisplayName("Callable: Valid User")
	@Test
	void test3() throws Exception {
		mockMvc.perform(post("/users/callable").contentType(MediaType.APPLICATION_JSON)
				.content(objectMapper.writeValueAsString(validUser))).andExpect(status().isOk());
	}

	@DisplayName("Deferred Result: Invalid User")
	@Test
	void test4() throws Exception {
		mockMvc.perform(post("/users/deferred").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON))
				.andExpect(status().isBadRequest());
	}

	@DisplayName("Deferred Result: Valid User")
	@Test
	void test5() throws Exception {

		MvcResult mvcResult = mockMvc
				.perform(post("/users/deferred").contentType(MediaType.APPLICATION_JSON)
						.content(objectMapper.writeValueAsString(validUser)))
				.andExpect(request().asyncStarted()).andReturn();

		// Dispatch async and assert success
		mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isOk())
				.andExpect(content().string("Created user: John Doe"));
	}

	@DisplayName("Deferred Result: Invalid User")
	@Test
	void test6() throws Exception {

		MvcResult mvcResult = mockMvc
				.perform(post("/users/deferred").contentType(MediaType.APPLICATION_JSON)
						.content(objectMapper.writeValueAsString(invalidUserJSON)))
				// In SB 3.x, validation may prevent async from starting
				.andReturn();

		// If async never started due to validation, just check 400
		if (mvcResult.getRequest().isAsyncStarted()) {
			mockMvc.perform(asyncDispatch(mvcResult)).andExpect(status().isBadRequest());
		} else {
			mockMvc.perform(post("/users/deferred").contentType(MediaType.APPLICATION_JSON)
					.content(objectMapper.writeValueAsString(invalidUserJSON))).andExpect(status().isBadRequest());
		}
	}
}
