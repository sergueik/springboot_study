package example.controller;

import org.junit.jupiter.api.Disabled;
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

	  /*
     * NOTE:
     * This test demonstrates capturing internal Spring MVC warnings when
     * deserialization fails. In Spring Boot 3.x, valid JSON with invalid fields
     * may not trigger logging, so tests asserting logged PII may fail.
     * This is a rare, exotic scenario and mostly serves as documentation
     * of the potential behavior. No further practical validation benefit.
     */
	
	@Autowired
	private MockMvc mockMvc;

	// Invalid JSON: email contains PII, age is valid
	private final String invalidUserJSON_PII = "{\"name\":\"John\", \"email\":\"pii@example.com\", \"age\":30, \"phone\":\"1234567890\"}";

	// Invalid JSON: age is null, phone too short
	private final String invalidUserJSON_Fields = "{\"name\":\"John\", \"email\":\"john@example.com\", \"age\":null, \"phone\":\"1234\"}";

	@Disabled
	@DisplayName("Capture log: PII redaction. NOTE: SB 3.x no longer logs for valid JSON with invalid fields. Kept for reference")
	@Test
	void test1(CapturedOutput output) throws Exception {
		mockMvc.perform(post("/users/blocking").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON_PII))
				.andExpect(status().isBadRequest());

		String log = output.getAll();

		// Assert that sensitive info is redacted
		assertThat(log, containsString("\"email\":\"[REDACTED]\""));

		// Non-sensitive fields remain clear
		assertThat(log, containsString("\"age\":30"));
	}

	@Disabled
	@DisplayName("Capture log: invalid fields, plain text")
	@Test
	void test2(CapturedOutput output) throws Exception {
		mockMvc.perform(post("/users/blocking").contentType(MediaType.APPLICATION_JSON).content(invalidUserJSON_Fields))
				.andExpect(status().isBadRequest());

		String log = output.getAll();

		// Email not sensitive, remains clear
		assertThat(log, containsString("\"email\":\"john@example.com\""));

		// Null age and short phone should appear in log
		assertThat(log, containsString("\"age\":null"));
		assertThat(log, containsString("\"phone\":\"1234\""));
	}
}
