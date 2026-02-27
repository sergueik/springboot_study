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

import example.controller.Controller;
import example.service.ExampleService;
import example.ExampleApplication;

@PropertySource("classpath:application.properties")
@WebMvcTest
public class MVCArrayValueTest {

	static String route = "/basic/params";
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
		route = "/basic/params";
		// TODO: find out what TCP port is listening during the test run
		// Assume.assumeTrue(listening("localhost", 8085));

	}

	// examine HTTP status and body - missing request param
	@Ignore("fails on Linux with a status 404")
	@Test
	public void test1() throws Exception {

		resultActions = mvc.perform(get(route));
		resultActions.andExpect(status().isMethodNotAllowed());
		resultActions.andExpect(content().string(containsString("[]")));
	}

	// examine HTTP status and body, value list argument
	// NOTE: old Spring Framework - incorrectly parses and is losing request param
	@Test
	@Ignore("fails on Linux with a status 404")
	public void test2() throws Exception {

		resultActions = mvc.perform(get(route + "?values=a&values=b&values=c"));
		resultActions.andExpect(status().isOk());
		resultActions.andExpect(content().string(containsString("[\"a\"]")));
	}

	// examine HTTP status and body, value list argument
	// NOTE: old Spring Framework - incorrectly parses and
	// is losing some request params
	@Ignore("fails on Linux with a status 404")
	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args1));
		resultActions.andExpect(status().isOk());

		resultActions.andExpect(content()
				.string(containsString(String.format("[\"%s\"]", values.get(0)))));
	}

	// NOTE: old Spring Framework - incorrectly parses and
	// is losing some request params
	@Ignore("fails on Linux with a status 404")
	@Test
	public void test4() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args1));
		resultActions.andExpect(jsonPath("$.*", hasSize(1)));
	}

	@Ignore("fails on Linux with a status 404")
	@Test
	public void test5() throws Exception {
		resultActions = mvc.perform(get(route + "?values=a,b,c"));
		resultActions.andExpect(status().isOk());
		resultActions
				.andExpect(content().string(containsString("[\"a\",\"b\",\"c\"]")));

	}

	@Ignore("fails on Linux with a status 404")
	@Test
	public void test6() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args2));
		resultActions.andExpect(jsonPath("$.*", hasSize(4)));
	}

	@Ignore("wrong matcher ?")
	@Test
	public void test7() throws Exception {
		resultActions = mvc.perform(get(route + "?" + args2));
		resultActions.andExpect(jsonPath("$.*", arrayWithSize(equalTo(4))));
	}

	@Ignore("fails on Linux with a status 404")
	@Test
	public void test8() throws Exception {
		charset = "UTF-8";
		mvc.perform(get(route + "?" + args1).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(
						String.format("application/json;charset=%s", charset)));
	}

	// @Ignore("Content type not set")
	@Ignore("fails on Linux with a status 404")
	@Test
	public void test9() throws Exception {
		mvc.perform(get(route + "?" + args1).accept(MediaType.TEXT_PLAIN))
				.andExpect(content().string("")).andExpect(status().isNotAcceptable())
				.andExpect(status().reason((String) null));
	}

	// examine value
	@Ignore("No value at JSON path \"$.length()\"")
	@Test
	public void test10() throws Exception {
		mvc.perform(get(route + "/json")).andExpect(jsonPath("$.length()", is(4)));
	}

	// examine value
	@Test
	public void test11() throws Exception {
		mvc.perform(get(route + "?" + args2)).andExpect(jsonPath("@[1]", is("b")));
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
