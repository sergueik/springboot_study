package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
//import org.junit.Before;
//import org.junit.Ignore;
//import org.junit.Test;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mozilla.javascript.CompilerEnvirons;
import org.mozilla.javascript.ErrorReporter;
import org.mozilla.javascript.EvaluatorException;
import org.mozilla.javascript.Parser;
import org.mozilla.javascript.ast.AstRoot;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

// import com.jayway.jsonpath.JsonPath;
// import com.jayway.jsonpath.Configuration;
// import com.jayway.jsonpath.InvalidPathException;
// import com.jayway.jsonpath.Option;

import example.entities.User;

// NOTE: incomplete: requires explicit start of the app in the sibling console otherwise having
// org.springframework.web.client.ResourceAccessException: I/O error on POST request for
// http://localhost:8085/basic/post/
// Connection refused: connect; nested exception is java.net.ConnectException: Connection refused: connect

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8084" })

public class RestTemplateTest {

	@LocalServerPort
	// NOTE: property annotations seem to have no effect here
	// @Value("${server.port:8085}")
	// @Value("${serverPort}")
	// private int randomServerPort;
	private int randomServerPort = 8084;

	public static final String baseUrl = "http://localhost:8084/";

	private RestTemplate restTemplate = new RestTemplate();
	private static HttpHeaders headers = new HttpHeaders();
	private static List<User> response = null;

	@BeforeAll
	public static void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	// @Disabled
	// org.h2.jdbc.JdbcSQLException: Database may be already in use: null. Possible
	// solutions: close all other connection(s); use the server mode
	@Test
	public void test1() {
		String route = "/api/users";
		String url = "http://localhost:" + randomServerPort + route;

		// String url = baseUrl + "api/users";
		response = restTemplate.getForObject(url, List.class);
		assertThat(response, notNullValue());
		// assertThat(response)
		// search = String.format("$.users[?(@.email == '%s' && @.company.department ==
		// '%s')].firstName", email, department);
// try {
		// result = JsonPath.parse(data).read(search).toString();
		// System.out.println("result: " + result);

		// } catch (InvalidPathException e) {
//		assertThat(e.getMessage(), containsString("Expected close predicate token"));
		// }
	}

	@Test
	public void test3() {

		EvaluatorException thrown = Assertions.assertThrows(EvaluatorException.class, () -> {
			String url = baseUrl + "inline.bundle.js";
			String page = restTemplate.getForObject(url, String.class);
			assertThat(page, notNullValue());
			System.err.println(page);
			String scriptString = "var x = 10; function greet() { return 'Hello'; }";

			// 1. Create a CompilerEnvirons instance
			CompilerEnvirons compilerEnv = new CompilerEnvirons();

			// 2. Create an ErrorReporter (can be customized)
			ErrorReporter errorReporter = compilerEnv.getErrorReporter();

			// 3. Create a Parser instance
			Parser parser = new Parser(compilerEnv, errorReporter);

			try {
				// 4. Parse the JavaScript string
				AstRoot astRoot = parser.parse(page, null, 1);

				if (astRoot != null) {
					System.out.println("Parsing successful! AST Root: " + astRoot.depth() + "\n" + astRoot.toSource());
					// You can further process the AST (e.g., traverse it to analyze the code)
				} else {
					System.err.println("Parsing failed.");
				}

			} catch (EvaluatorException e) {
				System.err.println("Error during parsing: " + e.getMessage());
				throw e;
			}

		});

		assertThat(thrown.getMessage(), is("missing ; before statement"));
	}

	@Test
	public void test2() {

		String url = baseUrl + "inline.bundle.js";
		String page = restTemplate.getForObject(url, String.class);
		assertThat(page, notNullValue());
		System.err.println(page);
		String scriptString = "var x = 10; function greet() { return 'Hello'; }";

		// 1. Create a CompilerEnvirons instance
		CompilerEnvirons compilerEnv = new CompilerEnvirons();

		// 2. Create an ErrorReporter (can be customized)
		ErrorReporter errorReporter = compilerEnv.getErrorReporter();

		// 3. Create a Parser instance
		Parser parser = new Parser(compilerEnv, errorReporter);

		try {
			// 4. Parse the JavaScript string
			AstRoot astRoot = parser.parse(page, null, 1);

			if (astRoot != null) {
				System.out.println("Parsing successful! AST Root: " + astRoot.depth() + "\n" + astRoot.toSource());
				// You can further process the AST (e.g., traverse it to analyze the code)
			} else {
				System.err.println("Parsing failed.");
			}

		} catch (EvaluatorException e) {
			System.err.println("Error during parsing: " + e.getMessage());
		}
	}

}
