package example.controller;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;

import example.Application;
import example.model.User;
import example.service.UserService;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

// note complex syntax to ensure bean creation
// instruct Spring Boot to load only the web layer
// and explicitly to a specific controller
// allow including extra beans that would otherwise not be scanned
// select beans by exact class name match
// provide the RestControllerAdvice class name to include in the test context

//@WebMvcTest(controllers = DeferredResultUserController.class, includeFilters = {
//		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = GlobalExceptionHandler.class) })

// @SpringBootTest
// Loads the entire Spring context
// note this is insufficient - fails to autowire the GlobalExceptionHandler class

@SpringBootTest(classes = { Application.class, GlobalExceptionHandler.class })
@AutoConfigureMockMvc
public class DeferredResultUserControllerDomainValidationTest {

	@Autowired
	private MockMvc mockMvc;

	// inject the real service into Spring context but allow selective stubbing
	@SpyBean
	private UserService userService;

	private ObjectMapper objectMapper = new ObjectMapper();

	private static String endpoint = "/deferred/users";
	private User user = null;

	// @Disabled
	@DisplayName("Bad Request when user is blank")
	@Test
	void test1() throws Exception {
		user = new User(1L, null, "dummy@example.com");
		// trigger name violation
		mockMvc.perform(
				post(endpoint).contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(user)))
				.andExpect(status().isBadRequest())
				.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
				.andExpect(jsonPath("$.name").exists()) // verify validation message is present
				.andExpect(jsonPath("$.email").value("dummy@example.com")); // peek into dummy email
	}

	// @Disabled
	@DisplayName("Bad Request when email is invalid")
	@Test
	void test2() throws Exception {
		// Arrange
		user = new User(1L, "Alice", "not-an-email");
		// trigger email format violation
		// Act, Assert
		mockMvc.perform(
				post(endpoint).contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(user)))
				.andExpect(status().isBadRequest())
				.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
				.andExpect(jsonPath("$.email").exists()).andExpect(jsonPath("$.name").value("Valid Name"))
				.andExpect(jsonPath("$.email").value("dummy@example.com")); // peek into dummy email

	}

	@DisplayName("Blocking Bad Request when email is invalid")
	@Test
	void test3() throws Exception {
		mockMvc.perform(post(endpoint + "/justvalidation").contentType("application/json")
				.content("{\"name\":\"\", \"email\":\"notemail\"}")).andExpect(status().isBadRequest());
	}
}
