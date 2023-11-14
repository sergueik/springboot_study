package example.controller;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assume;

import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.util.UriComponentsBuilder;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

import example.controller.Controller;
import example.service.ExampleService;
import example.ExampleApplication;
import com.google.gson.Gson;

// NOTE: @Runwith annotation with real classes crashes the JVM
// @RunWith(SpringJUnit4ClassRunner.class)
@PropertySource("classpath:application.properties")
@WebMvcTest
public class QueryControllerTest {

	private static String route = "/basic";
	final static String body = "Hello basic";
	private ResultActions resultActions;
	private static MockMvc mvc;
	private Map<String, String> params = new HashMap<>();
	private MvcResult mvcResult;
	private MockHttpServletResponse httpServletResponse;
	private String responseBody = null;
	private Map<String, String> data = new HashMap<>();
	private final Gson gson = new Gson();

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static ExampleApplication application = new ExampleApplication();
	private static QueryStringController controller = new QueryStringController();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	String id = "43";

	@Before
	public void beforeTest() throws Exception {
		route = "http://localhost:8085/basic/data/" + id;
		// NOTE: java.lang.IllegalArgumentException: [/basic/data/43] is not a valid
		// HTTP URL
		params.put("key1", "value 1");
		params.put("key2", "value 2");
		params.put("key3", "value 3");
		UriComponentsBuilder uiComponentsBuilder = UriComponentsBuilder
				.fromHttpUrl(route);
		for (String key : params.keySet()) {
			uiComponentsBuilder = uiComponentsBuilder.queryParam(key,
					"{" + key + "}");
		}

		String url = uiComponentsBuilder.toUriString();
		// String url = uiComponentsBuilder.replaceQueryParams(params);
		System.err.println("url(1): " + url);
		// see also: https://www.baeldung.com/java-url-encoding-decoding
		for (String key : params.keySet()) {
			url = url.replace("{" + key + "}", URLEncoder.encode(params.get(key)));
		}
		// url = "/basic/data/" + id;
		System.err.println("url(2): " + url);
		resultActions = mvc.perform(get(url));
	}

	// examine body
	// @Ignore
	@Test
	public void test1() throws Exception {
		resultActions.andExpect(status().isOk());
		mvcResult = resultActions.andReturn();
		httpServletResponse = mvcResult.getResponse();
		assertThat(httpServletResponse, notNullValue());
		responseBody = httpServletResponse.getContentAsString();
		assertThat(responseBody, notNullValue());
		data = gson.fromJson(responseBody, Map.class);
		assertThat(data, notNullValue());
		assertThat("Unexpected response for " + route, data.containsKey("id"),
				is(true));

	}
}
