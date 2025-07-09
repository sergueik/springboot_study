package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.CoreMatchers.containsString;
import org.json.JSONArray;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
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

import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.InvalidPathException;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.InvalidJsonException;

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
	private static String page = null;
	private String url = null;
	private String route = "/api/users";

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
		route = "/api/users";
		url = "http://localhost:" + randomServerPort + route;

		response = restTemplate.getForObject(url, List.class);
		assertThat(response, notNullValue());
		assertThat(response.size(), greaterThan(1));
		// TODO: try JSON constructor
		JSONArray json = new JSONArray(response);
		assertThat(json, notNullValue());
		assertThat(json.get(0), notNullValue());
	}

	@Test
	public void test2() {
		String route = "/api/users";
		String url = "http://localhost:" + randomServerPort + route;

		page = restTemplate.getForObject(url, String.class);
		assertThat(page, notNullValue());

		String search = String.format("$[?(@.fname == '%s')].lname", "one");
		String name = "one";

		try {
			String result = JsonPath.parse(page).read(search).toString();
			assertThat(result, containsString(name));
			System.out.println("returned: " + result);

		} catch (InvalidPathException e) {
			assertThat(e.getMessage(), containsString("Expected close predicate token"));
		}
	}

	@Disabled
	@Test
	public void test3() {

		EvaluatorException thrown = Assertions.assertThrows(EvaluatorException.class, () -> {
			url = "http://localhost:" + randomServerPort + "/inline.bundle.js";
			String page = restTemplate.getForObject(url, String.class);
			assertThat(page, notNullValue());

			CompilerEnvirons compilerEnv = new CompilerEnvirons();
			ErrorReporter errorReporter = compilerEnv.getErrorReporter();
			Parser parser = new Parser(compilerEnv, errorReporter);

			try {
				AstRoot ast = parser.parse(page, null, 1);

				if (ast != null) {
					System.out.println("Parsing successful! AST Root: " + ast.toSource());
					// NOTE: can further process the AST
					// e.g., traverse it to analyze the code
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

	// safer to run when the code is clean
	@Test
	public void test4() {

		String url = baseUrl + "inline.bundle.js";
		String page = restTemplate.getForObject(url, String.class);
		assertThat(page, notNullValue());
		CompilerEnvirons compilerEnv = new CompilerEnvirons();
		ErrorReporter errorReporter = compilerEnv.getErrorReporter();
		Parser parser = new Parser(compilerEnv, errorReporter);

		try {
			AstRoot ast = parser.parse(page, null, 1);

			if (ast != null) {
				System.out.println("Parsing successful! AST Root: " + ast.toSource());
				// NOTE: can further process the AST (e.g., traverse it to analyze the code)
			} else {
				System.err.println("Parsing failed.");
			}

		} catch (EvaluatorException e) {
			System.err.println("Error during parsing: " + e.getMessage());
			assertThat(e.getMessage(), is("missing ; before statement"));
			throw e;
		}
	}

}
