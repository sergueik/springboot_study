package example.controller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import example.controller.DataProcessingController;

import static org.mockito.Mockito.doThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
import org.springframework.security.test.context.support.WithMockUser;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.matchesRegex;
import static org.hamcrest.Matchers.is;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@ActiveProfiles("test")
@AutoConfigureMockMvc(addFilters = false)
@WebMvcTest(OperationController.class)
public class OperationControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@Test
	void test1() throws Exception {

		MvcResult result = mockMvc
				.perform(post("/operation").contentType("application/json").content("{ \"what\": \"hello\" }"))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.id").value(matchesRegex("^[0-9a-fA-F-]{36}$")))
				.andReturn();

		String json = result.getResponse().getContentAsString();

		ObjectMapper mapper = new ObjectMapper();
		JsonNode node = mapper.readTree(json);

		String id = node.get("id").asText();

		mockMvc.perform(put("/operation/" + id).contentType("application/json").content("{\"what\":\"updated\"}"))
				.andExpect(status().isOk());

		// NOTE: SOAP-like system may choose different statuses for
		// delete existing and
		// no-existing id
		mockMvc.perform(get("/operation/" + id)).andExpect(status().isOk())
				.andExpect(jsonPath("$.what").value(is("updated")));
		mockMvc.perform(delete("/operation/" + id)).andExpect(status().isNoContent());
	}

	@Test
	// NOTE: delete always succeeds, never fails
	void test2() throws Exception {
		mockMvc.perform(delete("/operation/999")).andExpect(status().isNoContent());
	}
}
