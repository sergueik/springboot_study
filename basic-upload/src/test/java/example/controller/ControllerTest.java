package example.controller;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.is;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import org.springframework.http.MediaType;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@WebMvcTest
public class ControllerTest {
	final static String route = "/basic/upload";
	private static MockMvc mvc;

	private static Controller controller = new Controller();
	private static MockMultipartFile file;
	private ResultActions resultActions;
	private MvcResult result;
	private MockHttpServletRequest request;

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		file = new MockMultipartFile("file", "test.txt", MediaType.TEXT_PLAIN_VALUE,
				"Hello, World!".getBytes());
	}

	// TODO: set up the failing scrnario

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc
				.perform(multipart(route).file(file).characterEncoding("utf-8"));
	}

	// examine response status and body
	@Test
	public void test1() throws Exception {
		resultActions.andExpect(status().isOk()).andExpect(content().string(""));
	}

	// examine request body
	// NOTE: encoding sensitive:
	// java.lang.IllegalStateException: Cannot get content as a String for a
	// null character encoding. Consider setting the characterEncoding in the
	// request.
	@Test
	public void test2() throws Exception {
		result = resultActions.andReturn();
		request = result.getRequest();
		assertThat(request.getContentAsString(), nullValue());
		List<String> headers = new ArrayList();
		Enumeration<String> headerNames = request.getHeaderNames();
		while (headerNames.hasMoreElements()) {
			String headerName = headerNames.nextElement();
			System.err.println("Added header name: " + headerName);
			headers.add(headerName);
			String headerValue = request.getHeader(headerName);
			System.err.println("Header value: " + headerValue);
		}
		assertThat(String.join(",", headers), is("Content-Type"));
		// not what was expected ?

	}

	// NOTE: cannot perform console capture test dure to spring boot version
	// conflicts:
	// in older one there is no multipart method,
	// in newer no capture
	// class
}
