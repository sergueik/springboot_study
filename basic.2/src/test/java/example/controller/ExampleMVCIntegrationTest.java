package example.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.service.ExampleService;

@WebMvcTest
public class ExampleMVCIntegrationTest {

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;

	final static String route = "/basic";
	final static String body = "Hello basic";
	final static String charset = "UTF-8";
	private ResultActions resultActions;

	@BeforeEach
	public void beforeTest() throws Exception {
		when(mockService.hello()).thenReturn(body);
		resultActions = mvc.perform(get(route));
	}

	@Test
	void alltest() throws Exception {
		resultActions.andDo(print()).andExpect(status().isOk())
				.andExpect(content().string(equalTo(body)));
		verify(mockService).hello();
	}

	// examine backend call
	@Test
	public void subjectMethodTest() throws Exception {
		verify(mockService).hello();
	}

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		resultActions.andExpect(content().string(containsString(body)));
	}

	// examine value
	@Test
	public void jsonTest() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.name", is(body)));
	}

	// count nodes
	@Test
	public void jsonTest2() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.*", hasSize(1)));
	}

	// examine response header
	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)).andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}
}
