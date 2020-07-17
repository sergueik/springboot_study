package example;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import org.springframework.test.context.junit4.SpringRunner;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import example.ExampleApplication;

@RunWith(SpringRunner.class)
@WebMvcTest(ExampleApplication.class)

// based on https://github.com/bytestree/spring-restful-service-unit-test
// see also:
// https://github.com/eugenp/tutorials/blob/master/spring-core-2/src/test/java/com/baeldung/startup/SpringStartupIntegrationTest.java
public class ExampleApplicationTests {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	ExampleApplication exampleApplication;

	private final String URL = "/basic";

	@Test
	public void test() throws Exception {
		// execute
		MvcResult result = mockMvc.perform(
				MockMvcRequestBuilders.get(URL).accept(MediaType.APPLICATION_JSON_UTF8))
				.andReturn();

		// verify status is OK
		assertEquals("Incorrect Response Status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
		// verify that mapped method was called
		verify(exampleApplication).hello();
	}
}
