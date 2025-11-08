package example.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;
import example.model.User;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(CallableForUserController.class)
class CallableForUserControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@Autowired
	private CallableForUserController controller;

	private final ObjectMapper objectMapper = new ObjectMapper();
	ResponseEntity<User> responseEntity = null;
	private Object objResponse;

	@BeforeEach
	void setup() {
		// Ensure user 1 exists before GET
		controller.init(); // must add User(1L, "Alice", "alice@example.com")
	}

	@DisplayName("GET /callable/users/1 returns User")
	@Test
	void testGetUserCallable() throws Exception {
		// Perform GET /callable/users/1
		MvcResult mvcResult = mockMvc.perform(get("/callable/users/1").contentType(MediaType.APPLICATION_JSON))
				.andExpect(request().asyncStarted()).andReturn();

		// Dispatch async result
		mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch(mvcResult))
				.andExpect(status().isOk()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(content().string(containsString("Alice")));

		// Get async result as JSON string
		objResponse = mvcResult.getAsyncResult(2000);
		assertThat(objResponse, notNullValue());

		// Deserialize response body to strongly typed User
		responseEntity = (ResponseEntity<User>) objResponse;
		User user = responseEntity.getBody();
		String jsonResponse = objectMapper.writeValueAsString(user);

		assertThat(user, notNullValue());
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), is("Alice"));

		// Use JsonPath to query the JSON
		String name = JsonPath.read(jsonResponse, "$.name");
		assertThat(name, is("Alice"));
	}
}
