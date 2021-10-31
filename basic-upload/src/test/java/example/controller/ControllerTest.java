package example.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@WebMvcTest
public class ControllerTest {
	final static String route = "/basic/upload";
	private static MockMvc mvc;
	private static String filename = "test.txt";
	private static String data = null;
	private static Controller controller = new Controller();
	private static MockMultipartFile file;
	private ResultActions resultActions;
	private MvcResult result;
	private MockHttpServletRequest request;

	@BeforeClass
	public static void setUp() throws IOException {
		String filePath = Paths.get(System.getProperty("user.dir"))
				.resolve(filename).toAbsolutePath().toString();
		data = new String(Files.readAllBytes(Paths.get(filePath)),
				Charset.forName("UTF-8"));

		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		// uses real file "test.txt"
		file = new MockMultipartFile("file", "test.txt", MediaType.TEXT_PLAIN_VALUE,
				data.getBytes());
	}

	// examine response status and body
	@Test
	public void test1() throws Exception {
		resultActions = mvc
				.perform(multipart(route + "?operation=send&param=something").file(file)
						.characterEncoding("utf-8"));
		resultActions.andExpect(status().isOk()).andExpect(content().string(data));
	}

	// examine response status and body
	@Test
	public void test2() throws Exception {
		resultActions = mvc.perform(multipart(route).file(file)
				.characterEncoding("utf-8").param("operation", new String[] { "send" })
				.param("param", new String[] { "non empty" }));
		resultActions.andExpect(status().isOk()).andExpect(content().string(data));
	}

	// examine request body
	// NOTE: encoding sensitive:
	// java.lang.IllegalStateException: Cannot get content as a String for a
	// null character encoding. Consider setting the characterEncoding in the
	// request.
	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(multipart(route).file(file)
				.characterEncoding("utf-8").param("operation", new String[] { "send" })
				.param("param", new String[] { "non empty" }));
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

	// examine response status
	@Test
	public void test4() throws Exception {
		resultActions = mvc.perform(
				multipart(route).file(file).characterEncoding("utf-8")
						.param("operation", new String[] { "unknown" }).param("param",
								new String[] { "non empty" }));
		resultActions.andExpect(status().isMethodNotAllowed());
	}

	// examine response status
	@Test
	public void test5() throws Exception {
		resultActions = mvc
				.perform(multipart(route).file(file).characterEncoding("utf-8")
						.param("operation", new String[] { "unknown" }));
		resultActions.andExpect(status().isBadRequest());
	}

	@Test
	public void test6() throws Exception {
		resultActions = mvc.perform(
				multipart(route).file(file).characterEncoding("utf-8")
						.param("operation", new String[] { "unknown" }).param("param",
								new String[] { "" }));
		resultActions.andExpect(status().isBadRequest());
	}

	// NOTE: version conflicts:
	// in older spring boot there is no multipart method,
	// in newer spring boot there is no console capture
	// class
}
