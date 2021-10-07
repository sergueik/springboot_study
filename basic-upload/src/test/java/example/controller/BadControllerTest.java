package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.ExampleApplication;

@WebMvcTest
public class BadControllerTest {
	final static String route = "/basic/upload";
	private ResultActions resultActions;
	private static MockMvc mvc;

	private static Controller controller = new Controller();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	// unfinished attempt to construct headers and body of the request
	// manually
	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(post(route).characterEncoding("utf-8")
				.contentType("multipart/form-data")
				.content(
						"--e7b7173e-70e0-418d-9982-35ae63b17c4a\r\nContent-Disposition: form-data; name=\"file\"; filename=\"temp.txt\"\r\nContent-Type: application/octet-stream\r\n\r\ntest data\r\n\r\n--e7b7173e-70e0-418d-9982-35ae63b17c4a--\r\n")
				.header("ContentType", new Object[] { "multipart/form-data",
						"boundary=\"a1abe753-03af-4116-b5b5-799781773e42\"" }));
	}

	// DEBUG
	// org.springframework.web.servlet.mvc.method.annotation.ServletInvocableHandlerMethod
	// -
	// Failed to resolve argument 0 of type
	// 'org.springframework.web.multipart.MultipartFile'
	// org.springframework.web.multipart.support.MissingServletRequestPartException:
	// Required request part 'file' is not present
	// @Ignore
	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isBadRequest());
	}

	// examine exception - none thrown 
	@Test
	public void exceptionTest() {
		try {
			resultActions.andExpect(content().string(""));
		} catch (Exception e) {
		//	System.err.println("Exception: " + e.toString());
		}
	}

}
