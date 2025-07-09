package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.util.List;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mozilla.javascript.CompilerEnvirons;
import org.mozilla.javascript.ErrorReporter;
import org.mozilla.javascript.EvaluatorException;
import org.mozilla.javascript.Parser;
import org.mozilla.javascript.ast.AstRoot;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

import example.entities.User;

// NOTE: incomplete: requires explicit start of the app in the sibling console otherwise having
// org.springframework.web.client.ResourceAccessException: I/O error on POST request for
// http://localhost:8085/basic/post/
// Connection refused: connect; nested exception is java.net.ConnectException: Connection refused: connect
public class RestTemplateTest {

	public static final String baseUrl = "http://localhost:8084/";

	private RestTemplate restTemplate = new RestTemplate();
	private HttpHeaders headers = new HttpHeaders();
	private List<User> response = null;

	@Before
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
	}

	@Ignore
	// org.h2.jdbc.JdbcSQLException: Database may be already in use: null. Possible
	// solutions: close all other connection(s); use the server mode
	@Test
	public void test1() {
		String url = baseUrl + "api/users";
		response = restTemplate.getForObject(url, List.class);
		assertThat(response, notNullValue());
	}

	@Test
	public void test2() {
		String url = baseUrl + "inline.bundle.js";
		String page = restTemplate.getForObject(url, String.class);
		assertThat(page, notNullValue());

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
