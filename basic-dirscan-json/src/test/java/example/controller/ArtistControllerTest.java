package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.PropertySource;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasKey;
import static org.junit.Assert.assertTrue;

import static org.hamcrest.Matchers.containsString;

import static org.hamcrest.Matchers.hasSize;
// import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import static org.hamcrest.Matchers.equalTo;
// import org.hamcrest.collection.*;
import static org.hamcrest.collection.IsArrayWithSize.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assume;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.google.gson.Gson;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/result/JsonPathResultMatchers.html
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
// requires a later version ?
// import static org.springframework.test.web.servlet.result.JsonPathResultMatchers.isArray;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import example.controller.DataController;
import example.Launcher;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;
import org.json.XML;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class ArtistControllerTest {

	static String route = "/data/";
	final static String body = "basic";
	private static final String name = "john";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;
	private static final Gson gson = new Gson();

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static Launcher application = new Launcher();
	private static DataController controller = new DataController();

	private final static List<String> files = Arrays.asList("dummy1.txt",
			"dummy2.txt");
	private static List<String> entries = new ArrayList<>();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
		// TODO: find out what TCP port is listening during the test run
		// Assume.assumeTrue(listening("localhost", 8085));
		route = "/data" + "/" + name;
		resultActions = mvc.perform(get(route));
	}

	@Test
	public void test4() throws Exception {
		// number of JSON keys
		int numkeys = 3;
		resultActions.andExpect(jsonPath("$.*", hasSize(numkeys)))
				.andExpect(jsonPath("$.*~", hasSize(numkeys)))
				.andExpect(jsonPath("$.length()", is(numkeys)));
	}

	// TODO: finish key extraction e.g. to expect id to be the last key
	@Test
	public void test5() throws Exception {
		String content = resultActions.andReturn().getResponse()
				.getContentAsString();
		assertThat(content,
				is("{\"name\":\"john\",\"plays\":\"instrument\",\"id\":1}"));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void test6() throws Exception {
		String content = resultActions.andReturn().getResponse()
				.getContentAsString();
		System.err.println("Content: " + content);
		final JSONObject result = new JSONObject(content);
		String key = null;
		Iterator<String> keys = result.keys();
		// NOTE: can get *first* key. Can get the *last* key
		// unfortunately the key come in random order - unreliable
		while (keys.hasNext()) {
			key = keys.next();
			System.err.println("Artist key: " + key);
		}
		assertThat(key, is("id"));
		assertThat(result.has("id"), is(true));
	}

	@Test
	public void test7() throws Exception {

		resultActions.andExpect(content().string(containsString("name")))
				.andExpect(jsonPath("$.name", is(name)));
	}

	// TODO: a better test
	// No value at JSON path "$.price", exception:
	// No results for path: $['price']
	@Ignore
	@Test(expected = AssertionError.class)
	public void test8() throws Exception {
		resultActions.andExpect(jsonPath("$.price", nullValue()));
	}

}
