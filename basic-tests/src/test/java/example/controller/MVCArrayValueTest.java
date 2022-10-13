package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsArrayWithSize.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assumptions;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import example.controller.ExampleController;
import example.service.ExampleService;
import example.Application;

@WebMvcTest
public class MVCArrayValueTest {

	static String route = null;
	final static String body = "Hello basic";

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;
	private ResultActions resultActions;

	final static List<String> values = Arrays
			.asList(new String[] { "a", "b", "c", "d" });
	final static String args1 = "values=x";
	final static String args2 = String.format("values=%s",
			String.join(",", values));
	final static String args3 = String.format("values=%s",
			String.join(",", values));

	@BeforeAll
	public static void setUp() {
	}

	@BeforeEach
	public void beforeTest() throws Exception {
		route = "/basic/array/params";
		// TODO: find out what TCP port is listening during the test run
		// Assumptions.assumeTrue(listening("localhost", 8085));

	}

	// examine HTTP status and body - missing request params
	@Test
	public void test1() throws Exception {

		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isMethodNotAllowed());
		resultActions.andExpect(content().string(containsString("[]")));
	}

	// examine HTTP status and body, value list argument
	// NOTE: old Spring Framework - incorrectly parses and is losing request param
	@Test
	public void test2() throws Exception {

		resultActions = mvc.perform(get(route + "?values=a&values=b&values=c"));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString("[\"a\",\"b\",\"c\"]")));
	}

	// single value argument converted to a list
	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args1));
		resultActions.andExpect(status().isOk());
		resultActions.andExpect(content().string(containsString("[\"x\"]")));
	}

	// examine HTTP status and body, value list argument
	@Test
	public void test4() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args2));
		resultActions.andExpect(status().isOk());

		resultActions.andExpect(content()
				.string(containsString(String.format("[\"%s\",\"%s\",\"%s\",\"%s\"]",
						values.get(0), values.get(1), values.get(2), values.get(3)))));
	}

	@Test
	public void test5() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args2));
		resultActions.andExpect(jsonPath("$.*", hasSize(4)));
	}

	@Test
	public void test6() throws Exception {
		resultActions = mvc.perform(get(route + "?values=a,b,c"));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString("[\"a\",\"b\",\"c\"]")));
	}

	@Test
	public void test7() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args3));
		resultActions.andExpect(jsonPath("$.*", hasSize(4)));
	}

	@Disabled("wrong matcher ?")
	@Test
	public void test8() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args3));
		resultActions.andExpect(jsonPath("$.*", arrayWithSize(equalTo(4))));
	}

	@Disabled("nothing was thrown")
	@Test
	public void test9() throws Exception {
		AssertionError thrown = assertThrows(AssertionError.class, () -> {
			mvc.perform(get(route + "?" + args2).accept(MediaType.TEXT_PLAIN))
					.andExpect(content().string(""));
		});
		assertThat("Expected Content type not set", thrown.getMessage(),
				containsString("Stuff"));
	}

	@Test
	// https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/406
	public void test10() throws Exception {
		mvc.perform(get(route + "?" + args2).accept(MediaType.TEXT_PLAIN))
				.andExpect(content().string("")).andExpect(status().isNotAcceptable());
	}

	@Test
	// NOTE: these expectations are Junit version sensitive
	public void test11() throws Exception {
		mvc.perform(get(route + "?" + args2).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}

	// examine value
	// @Disabled("No value at JSON path \"$.length()\"")
	@Test
	public void test12() throws Exception {
		mvc.perform(get(route + "?" + args2))
				.andExpect(jsonPath("$.length()", is(values.size())));
	}

	// examine value
	@Test
	public void test13() throws Exception {
		mvc.perform(get(route + "?" + args3)).andExpect(jsonPath("@[1]", is("b")));
	}

	// http://www.java2s.com/example/java-utility-method/http-port-find/isserverlistening-string-host-int-port-2d6d3.html
	@SuppressWarnings("unused")
	private static boolean listening(String host, int port) {
		Socket socket = null;
		try {
			socket = new Socket(host, port);
			return true;
		} catch (Exception e) {
			return false;
		} finally {
			if (socket != null) {
				try {
					socket.close();
				} catch (Exception e) {
					// ignore
				}
			}
		}
	}
}
