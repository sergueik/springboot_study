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
	private ResponseEntity<User> responseEntity = null;
	private String jsonResponse = null;
	private Object objResponse;
	private User user = null;
	private String name = null;
	
	@BeforeEach
	void setup() {
		// Ensure user 1 exists before GET
		controller.init();
	}

	@SuppressWarnings("unchecked")
	@DisplayName("GET /callable/users/1 returns User")
	@Test
	void test1() throws Exception {
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

		// NOTE:
		// mvcResult.getAsyncResult().toString();
		// <200 OK OK,example.model.User@4a864d4d,[]>
		// System.err.println("objResponse: " + mvcResult.getAsyncResult().toString());
		// Deserialize response body to strongly typed User
		responseEntity = (ResponseEntity<User>) objResponse;
		user = responseEntity.getBody();

		assertThat(user, notNullValue());
		assertThat(user.getId(), is(1L));
		assertThat(user.getName(), is("Alice"));

		// Use JsonPath to query the JSON
		jsonResponse = objectMapper.writeValueAsString(user);
		name = JsonPath.read(jsonResponse, "$.name");
		assertThat(name, is("Alice"));
	}
}
