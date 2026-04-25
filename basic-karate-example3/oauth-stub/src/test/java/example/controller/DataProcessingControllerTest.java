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

import example.controller.DataProcessingController;

import static org.mockito.Mockito.doThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.ArrayList;
import java.util.List;

@ActiveProfiles("test")
@AutoConfigureMockMvc(addFilters = false)
@WebMvcTest(DataProcessingController.class)
public class DataProcessingControllerTest {

	@Autowired
	private MockMvc mockMvc;
	// @WithMockUser(username = "user")
	@Test
	void test1() throws Exception {

		 mockMvc.perform(post("/api/processdata")
                 .contentType("application/json")
                 .content("{ \"customer\": \"test\", \"what\": \"hello\" }"))
         .andExpect(status().isOk())
         .andExpect(jsonPath("$.customerId").value("test"))
         .andExpect(jsonPath("$.messageType").value("processed"))
         .andExpect(jsonPath("$.payload.originalWhat").value("hello"))
         .andExpect(jsonPath("$.payload.length").value(5));
	}
}
