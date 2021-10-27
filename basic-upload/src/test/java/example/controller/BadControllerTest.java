package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.Application;

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
				.content(String.join("\r\n",
						Arrays.asList("--boundary",
								"Content-Disposition: form-data; name=\"file\"; filename=\"temp.txt\"",
								"Content-Type: application/octet-stream", "", "test data", "",
								"--boundary--", "")))
				.contentType("multipart/form-data; boundary=\"boundary\""));
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

	// examine exception - none is thrown
	@Test
	public void exceptionTest() {
		try {
			resultActions.andExpect(content().string(""));
		} catch (Exception e) {
			// System.err.println("Exception: " + e.toString());
		}
	}

}
