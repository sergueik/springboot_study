package example.controller;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import example.controller.Controller;
import example.service.ExampleService;
import example.ExampleApplication;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class MVCHostDataTest {

	static String route = "/basic/listdata/";
	final static String body = "Hello basic";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static ExampleApplication application = new ExampleApplication();
	private static ExampleService service = new ExampleService();
	private static Controller controller = new Controller(service);
	final static List<String> values = Arrays
			.asList(new String[] { "a", "b", "c", "d" });
	final static String args1 = String.join("&", values.stream()
			.map(o -> String.format("values=%s", o)).collect(Collectors.toList()));
	final static String args2 = String.format("values=%s",
			String.join(",", values));

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
		route = "/basic/listdata/localhost";
		// TODO: find out what TCP port is listening during the test run
		// Assume.assumeTrue(listening("localhost", 8085));

	}

	// arguments are ignored
	@Test
	public void test4() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args1));
		resultActions.andExpect(jsonPath("$.*", hasSize(2)));
	}

	@Test
	public void test10() throws Exception {
		mvc.perform(get(route)).andExpect(jsonPath("$.length()", is(2)));
	}

	@Test
	public void test11() throws Exception {
		mvc.perform(get(route)).andExpect(jsonPath("@[1]",
				is("C:\\developer\\sergueik\\springboot_study\\basic\\dummy2.txt")));
	}

	@Test
	public void test5() throws Exception {
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		List<String> files = Arrays.asList("dummy1.txt", "dummy2.txt");
		List<String> entries = files.stream().map((String filename) -> {
			final Path filePath = Paths
					.get(String.join(System.getProperty("file.separator"),
							Arrays.asList(System.getProperty("user.dir"), filename)));
			return filePath;
		}).map((Path filePath) -> filePath.toString()).collect(Collectors.toList());
		final Gson gson = new Gson();
		// ["C:\\developer\\sergueik\\springboot_study\\basic\\dummy1.txt","C:\\developer\\sergueik\\springboot_study\\basic\\dummy2.txt"]
		resultActions
				.andExpect(content().string(containsString(gson.toJson(entries))));

		// hand written JSON building for returned payload matcher will fail
		// resultActions.andExpect(content().string(containsString(
		// String.format("%s", entries.toString()).replaceAll("\\\\", "\\\\"))));

	}
}
