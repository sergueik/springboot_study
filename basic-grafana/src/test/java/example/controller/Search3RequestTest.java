package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.any;

import com.google.gson.Gson;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.HashMap;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.util.Base64Utils;
import java.util.Base64;

import example.Application;
import example.component.SearchRequest;
import example.service.ExampleService;

// NOTE: uncommenting the @Runwith annotation will crash the JVM
// with massive  IllegalState Failed to load ApplicationContext
// Parameter 0 of constructor in example.controller.SearchController required a bean of type 'example.service.ExampleService' that could not be found.
// @RunWith(SpringJUnit4ClassRunner.class)
@WebMvcTest
public class Search3RequestTest {

	static String route = null;
	final static String body = "text data";
	private final static String param = capitalize("param");
	private final static String value = "foo:bar";
	private final static String encodedValue = Base64Utils.encodeToString(value.getBytes());
	private static MockMvc mvc;
	private ResultActions resultActions;

	@SuppressWarnings("unused")
	private static Application application = new Application();
	@Mock
	private static ExampleService mockService = Mockito.mock(ExampleService.class);

	@InjectMocks
	private static SearchController controller = new SearchController(mockService);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
		when(mockService.getDataMap(value)).thenReturn(new HashMap<String, Object>());

	}

	// examine response headers
	@Test
	public void test1() throws Exception {
		route = "/search3";
		resultActions = mvc.perform(post(route).accept(MediaType.APPLICATION_JSON).content("{}")
				.header(param, encodedValue).contentType(MediaType.APPLICATION_JSON));
		resultActions.andExpect(header().string(param, encodedValue));
	}

	// examine response headers
	@Test
	public void test2() throws Exception {
		route = "/search4";
		resultActions = mvc.perform(post(route).accept(MediaType.APPLICATION_JSON).content("{}")
				.header(param, encodedValue).contentType(MediaType.APPLICATION_JSON));
		resultActions.andExpect(header().string(param, encodedValue));
	}

	// examine response headers
	@Test
	public void test3() throws Exception {
		route = "/search3";

		resultActions = mvc.perform(
				post(route).accept(MediaType.APPLICATION_JSON).content("{}").contentType(MediaType.APPLICATION_JSON));
		resultActions.andExpect(header().doesNotExist(param));
	}

	@Test
	public void test5() throws Exception {
		resultActions = mvc.perform(
				post(route).accept(MediaType.APPLICATION_JSON).content("{}").contentType(MediaType.APPLICATION_JSON));
		verify(mockService, atLeast(1)).getDataMap(any());
	}

	// https://www.baeldung.com/java-base64-encode-and-decode
	@Test
	public void test6() throws Exception {
		byte[] rawData = Base64.getDecoder().decode(encodedValue);
		assertThat(String.format("Cannot decode \"%s\" to \"%s\"", encodedValue, value), new String(rawData),
				is(value));

		rawData = Base64Utils.decode(encodedValue.getBytes());
		assertThat(String.format("Cannot decode  \"%s\" to \"%s\"", encodedValue, value), new String(rawData),
				is(value));

	}

	private static String capitalize(final String string) {
		if (string == null)
			throw new NullPointerException("null argument");
		if (string.equals(""))
			throw new NullPointerException("empty argument");

		return Character.toUpperCase(string.charAt(0)) + string.substring(1);
	}

}
