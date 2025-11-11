package example.controller;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.system.CapturedOutput;
import org.springframework.boot.test.system.OutputCaptureExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.containsString;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(UserController.class)
@ExtendWith(OutputCaptureExtension.class)
public class ExoticLoggingTest {

	@Autowired
	private MockMvc mockMvc;

	private final String invalidUserJSON = "{\"name\":\"\",\"email\":\"invalid-email\"}";

	@DisplayName("Capture internal Spring MVC warning log from invalid JSON deserialization")
	@Test
	void test(CapturedOutput output) throws Exception {
		mockMvc.perform(post("/users/blocking").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON))
				.andExpect(status().isBadRequest());

		// Assert that Spring MVC logged the warning

		assertThat(output.getAll(), containsString("HandlerExceptionResolver"));
		assertThat(output.getAll(), containsString("JSON parse error"));

	}
}
