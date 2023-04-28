package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;
// import org.hamcrest.collection.*;
import static org.hamcrest.collection.IsArrayWithSize.*;

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
import java.util.List;
import java.util.stream.Collectors;

import example.controller.DataController;
import example.Launcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class DataControllerTest {

	static String route = "/listdata/";
	final static String body = "Hello basic";
	private static final String hostname = "localhost";
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

	@BeforeAll
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@BeforeEach
	public void beforeTest() throws Exception {

		// TODO: find out what TCP port is listening during the test run
		// Assume.assumeTrue(listening("localhost", 8085));

	}

	// arguments are ignored
	@Test
	public void test4() throws Exception {
		route = "/listdata" + "/" + hostname;
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(jsonPath("$.*", hasSize(2)));
	}

	// arguments are ignored
	@Test
	public void test3() throws Exception {
		route = "/listdata" + "/" + "host";
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk())
				.andExpect(jsonPath("$.*", hasSize(0)));
	}

	@Test
	public void test10() throws Exception {
		route = "/listdata" + "/" + hostname;

		mvc.perform(get(route)).andExpect(status().isOk())
				.andExpect(jsonPath("$.length()", is(2)));
	}

	@Test
	public void test11() throws Exception {
		route = "/listdata" + "/" + hostname;
		mvc.perform(get(route)).andExpect(jsonPath("@[1]",

				is(String.join(System.getProperty("file.separator"),
						Arrays.asList(System.getProperty("user.dir"), files.get(1))))));
	}

	@Test
	public void test5() throws Exception {
		route = "/listdata" + "/" + hostname;
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		entries = files.stream().map((String filename) -> {
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

	@Test
	public void test6() throws Exception {
		route = "/listfilenames" + "/" + hostname;
		entries.clear();
		entries.addAll(files);
		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString(gson.toJson(entries))));

		// hand written JSON building for returned payload matcher will fail
		// resultActions.andExpect(content().string(containsString(
		// String.format("%s", entries.toString()).replaceAll("\\\\", "\\\\"))));

	}
}
