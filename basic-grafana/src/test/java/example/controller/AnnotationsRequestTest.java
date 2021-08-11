package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.controller.DataSoureController;
import example.service.ExampleService;
import example.Application;
import example.component.Annotation;
import example.component.AnnotationRequest;
import example.component.Range;
import example.component.SearchRequest;
import com.google.gson.Gson;

// NOTE: uncommenting the @Runwith annotation will crash the JVM
// with massive  IllegalState Failed to load ApplicationContext
// Parameter 0 of constructor in example.controller.DataSoureController required a bean of type 'example.service.ExampleService' that could not be found.
// @RunWith(SpringJUnit4ClassRunner.class)
@WebMvcTest
public class AnnotationsRequestTest {

	final static String route = "/annotations";
	final static String body = "annotation";
	private static String charset = null;
	private static final Gson gson = new Gson();
	private ResultActions resultActions;
	private static MockMvc mvc;

	private final AnnotationRequest annotationRequest = new AnnotationRequest();
	private final Annotation annotation = new Annotation();
	private final Range range = new Range();

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static Application application = new Application();
	private static ExampleService service = new ExampleService();
	private static AnnotationsController controller = new AnnotationsController(
			service);

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Before
	public void beforeTest() throws Exception {
		annotation.setDatasource("data");
		annotation.setEnable(true);
		annotation.setName("name");
		annotationRequest.setAnnotation(annotation);
		range.setFrom("from");
		range.setTo("to");

		annotationRequest.setRange(range);
		resultActions = mvc.perform(post(route).accept(MediaType.APPLICATION_JSON)
				.content(gson.toJson(annotationRequest))
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
		resultActions.andExpect(jsonPath("$[0].annotation.name", is("name")));
	}

	// count nodes
	// TODO: No value at JSON path "$.*", exception: json can not be null or empty
	@Test
	public void jsonTest2() throws Exception {
		resultActions.andExpect(jsonPath("$.*", hasSize(2)));
	}

	@Test
	// TODO : confirm rejections
	// NOTE: these expectations are Junit version sensitive
	public void rejectedMethodTest() throws Exception {
		charset = "ISO-8859-1";
		mvc.perform(get(route)).andExpect(status().isMethodNotAllowed());
	}

	@Test
	public void rejectionsTest() throws Exception {
		mvc.perform(post(route).contentType(MediaType.TEXT_PLAIN))
				.andExpect(status().isOk());
		mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isBadRequest());
		charset = "UTF-8";
		mvc.perform(post(route)
				.contentType(String.format("application/json;charset=%s", charset)))
				.andExpect(status().isBadRequest());
	}

	// examine response header
	@Test
	public void contentTypeTest() throws Exception {
		charset = "UTF-8";
		// NOTE: these expectations are Junit version sensitive
		mvc.perform(post(route).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(
						String.format("application/json;charset=%s", charset)));
	}

	@Test(expected = AssertionError.class)
	// application/json;charset=UTF-8
	public void contentTypeCharsetTest() throws Exception {
		mvc.perform(post(route).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

}
