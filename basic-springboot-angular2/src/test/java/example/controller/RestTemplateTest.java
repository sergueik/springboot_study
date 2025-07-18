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

import javax.annotation.PostConstruct;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mozilla.javascript.CompilerEnvirons;
import org.mozilla.javascript.ErrorReporter;
import org.mozilla.javascript.EvaluatorException;
import org.mozilla.javascript.Parser;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.ast.AstRoot;
import org.mozilla.javascript.Context;


import org.springframework.beans.factory.annotation.Value;
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


	@Value("${example.version:VERSION_1_8}")
	private String version;

	private int _version;

	@PostConstruct
	public void init() {
	    this._version = RhinoVersion.valueOf(version).getValue();
	}
	
	public static final String baseUrl = "http://localhost:8084/";

	private RestTemplate restTemplate = new RestTemplate();
	private static HttpHeaders headers = new HttpHeaders();
	private static List<User> response = null;
	private static String page = null;
	private String url = null;
	private String route = "/api/users";
	private static Context context = null;
	private static CompilerEnvirons compilerEnv = null;
	private static ErrorReporter errorReporter = null;
	private static Parser parser = null;
	private static AstRoot ast = null;
	private static Script script = null;
	private static org.json.JSONArray json = new org.json.JSONArray();

	@BeforeAll
	public static void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		context = Context.enter();
		// context.setLanguageVersion(version);
		compilerEnv = new CompilerEnvirons();
		errorReporter = compilerEnv.getErrorReporter();
		parser = new Parser(compilerEnv, errorReporter);
	}

	@Test
	public void test1() {
		route = "/api/users";
		url = "http://localhost:" + randomServerPort + route;

		response = restTemplate.getForObject(url, List.class);
		assertThat(response, notNullValue());
		assertThat(response.size(), greaterThan(1));
		// TODO: try JSON constructor
		json = new JSONArray(response);
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

	// @Disabled
	@Test
	public void test3() {

		EvaluatorException thrown = Assertions.assertThrows(EvaluatorException.class, () -> {
			url = "http://localhost:" + randomServerPort + "/inline.bundle.js";
			String page = restTemplate.getForObject(url, String.class);
			assertThat(page, notNullValue());

			try {
				ast = parser.parse(page, null, 1);
				System.out
						.println((ast != null) ? "Parsing successful! AST Root: " + ast.toSource() : "Parsing failed.");
			} catch (EvaluatorException e) {
				System.err.println("Error during parsing: " + e.getMessage());
				throw e;
			}

		});

		assertThat(thrown.getMessage(), is("missing ; before statement"));
	}

	// @Disabled
	@Test
	public void test4() throws NumberFormatException {

		String url = baseUrl + "inline.bundle.js";
		String page = restTemplate.getForObject(url, String.class);
		assertThat(page, notNullValue());
		EvaluatorException thrown = Assertions.assertThrows(EvaluatorException.class, () -> {
			try {
				try {
					System.out.println("XXXXXX setLanguageVersion: " + version);
					context.setLanguageVersion(_version);
				} catch (NumberFormatException e) {
					System.err.println("NumberFormatException: " + e.getMessage());
					throw e;
					// getLanguageVersion()
				}
				script = context.compileString(page, null, 1, null);
				System.out.println((script != null) ? "Parsing successful: " + script.toString() : "Parsing failed.");

			} catch (EvaluatorException e) {
				System.err.println("Error during parsing: " + e.getMessage());
				assertThat(e.getMessage(), is("missing ; before statement"));
				throw e;
			}
		});
		assertThat(thrown.getMessage(), is("missing ; before statement"));
	}

}

