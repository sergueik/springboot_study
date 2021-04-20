package example.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatcher;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.any;
import org.mockito.ArgumentMatcher;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.service.ExampleService;

import com.google.gson.Gson;

@WebMvcTest
public class ExampleMVCIntegrationTest {

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;

	final static String route = "/basic";
	final static String body = "Hello basic";
	final static String charset = "UTF-8";
	final ExampleController.Data data = new ExampleController.Data("data");
	private ResultActions resultActions;
	private static final Gson gson = new Gson();

	@BeforeEach
	public void beforeTest() throws Exception {
		when(mockService.hello()).thenReturn(body);
		when(mockService.handleData(data)).thenReturn(data);

		resultActions = mvc.perform(get(route));
	}

	@Test
	void alltTest() throws Exception {
		resultActions.andDo(print()).andExpect(status().isOk()).andExpect(content().string(equalTo(body)));
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
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN))
				.andExpect(content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}

	@Test
	void postTest() throws Exception {
		String content = gson.toJson(data);
		mvc.perform(post(route + "/post").contentType("application/json").content(content)).andDo(print())
				.andExpect(status().isOk()).andExpect(jsonPath("$.name", is(data.getName())));

		// https://www.baeldung.com/mockito-argument-matchers
		verify(mockService).handleData(any(ExampleController.Data.class));

		// see also https://www.codota.com/code/java/methods/org.mockito.Mockito/argThat
		
		// Wanted but not invoked:
		// Actual invocations have different arguments:
		/*
		verify(mockService).handleData(argThat(new ArgumentMatcher<ExampleController.Data>() {
			@Override
			public boolean matches(ExampleController.Data argument) {
				System.err.println("Simple name: " + argument.getClass().getSimpleName());
				return argument.getClass().getSimpleName().equals("ExampleController.Data");
			}
		}));
		// TODO: lambda
		verify(mockService)
				.handleData(argThat(argument -> argument.getClass().getSimpleName().equals("ExampleController.Data")));
				*/
	}

}
