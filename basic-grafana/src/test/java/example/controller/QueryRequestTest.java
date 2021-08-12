package example.controller;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.junit.Before;
import org.junit.BeforeClass;
// import org.junit.Ignore;
import org.junit.Test;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.google.gson.Gson;

import example.Application;
import example.component.Annotation;
import example.component.AnnotationRequest;
import example.component.QueryRequest;
import example.component.Range;
import example.component.TargetObj;
import example.service.ExampleService;
import static org.hamcrest.Matchers.greaterThan;

@WebMvcTest
public class QueryRequestTest {

	final static String route = "/query";
	final static String body = "target";
	private static String charset = null;
	private static final Gson gson = new Gson();
	private ResultActions resultActions;
	private static MockMvc mvc;

	private final QueryRequest queryRequest = new QueryRequest();
	private final TargetObj targetObj = new TargetObj();
	private final Range range = new Range();

	@SuppressWarnings("unused")
	private static Application application = new Application();
	private static ExampleService service = new ExampleService();
	private static QueryController controller = new QueryController(service);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {

		targetObj.setTarget(body);
		queryRequest.setTargets(Arrays.asList(targetObj));
		range.setFrom("from");
		range.setTo("to");

		queryRequest.setRange(range);
		resultActions = mvc.perform(post(route).accept(MediaType.APPLICATION_JSON)
				.content(gson.toJson(queryRequest))
				.contentType(MediaType.APPLICATION_JSON));
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

	@Test
	public void jsonTest() throws Exception {
		resultActions
				.andExpect(jsonPath("$[0].datapoints[0][0]", greaterThan((double) 0)));
		// TODO: finish examine extracted JSON
		String result = resultActions.andReturn().getResponse()
				.getContentAsString();

	}

	// count nodes
	// TODO: No value at JSON path "$.*", exception: json can not be null or empty
	@Test
	public void jsonTest2() throws Exception {
		resultActions.andExpect(jsonPath("$.*", hasSize(1)));
	}

}
